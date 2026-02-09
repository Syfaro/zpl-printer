use std::{net::SocketAddr, num::NonZeroUsize, sync::Arc};

use age::secrecy::ExposeSecret;
use axum::{Router, middleware};
use axum_prometheus::PrometheusMetricLayer;
use axum_server::tls_rustls::{RustlsAcceptor, RustlsConfig};
use clap::{Parser, Subcommand};
use ipp::prelude::{AsyncIppClient, Uri};
use lru::LruCache;
use rustls::{ServerConfig, crypto, server::WebPkiClientVerifier};
use sea_orm::{Database, DatabaseConnection};
use tokio::sync::Mutex;
use tokio_util::{sync::CancellationToken, task::TaskTracker};
use tower_http::trace::{DefaultOnRequest, DefaultOnResponse};
use tracing::{Level, info};

use migration::{Migrator, MigratorTrait};
use tracing_subscriber::EnvFilter;

use crate::print::Print;

mod alerts;
#[allow(unused_imports)]
mod entities;
mod print;
mod template;
mod transport;
mod web;
mod weblink;
mod zpl;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    if std::env::var_os("RUST_LOG").is_none() {
        unsafe { std::env::set_var("RUST_LOG", "info,sqlx=warn,tower_http::trace=debug") }
    }

    let config = Config::parse();

    if config.pretty_logs {
        tracing_subscriber::fmt::fmt()
            .pretty()
            .with_env_filter(EnvFilter::from_default_env())
            .init();
    } else {
        tracing_subscriber::fmt::init();
    }

    match config.command {
        Command::GenerateSecret => println!(
            "{}",
            age::x25519::Identity::generate()
                .to_string()
                .expose_secret()
        ),
        Command::GenerateCa(ca_config) => {
            let cert = weblink::generate_ca(ca_config).await?;

            println!("{}", cert.id);
        }
        Command::Serve(run_config) => run(*run_config).await?,
    }

    Ok(())
}

async fn prepare_db(url: &str) -> eyre::Result<DatabaseConnection> {
    let db = Database::connect(url).await?;
    Migrator::up(&db, None).await?;
    Ok(db)
}

async fn run(run_config: RunConfig) -> eyre::Result<()> {
    let db = prepare_db(&run_config.database_url).await?;

    let ipp_client = if let Some(cups_host) = run_config.cups_host {
        info!("enabling cups");

        Some(Arc::new(AsyncIppClient::builder(cups_host).build()))
    } else {
        None
    };

    let (handle, token) = shutdown_signal().await;

    let weblink_state = if run_config.weblink.enable {
        let root_ca = weblink::get_root_ca(&db, run_config.weblink.root_certificate_id.unwrap())
            .await?
            .ok_or_else(|| eyre::eyre!("could not find root certificate"))?;

        Some(Arc::new(web::WebLinkState {
            root_ca,
            root_encryption_identity: run_config.weblink.identity()?,
            transport: Box::new(transport::InMemoryTransport::new()),
            tasks: TaskTracker::new(),
        }))
    } else {
        None
    };

    let mut alert_handler =
        alerts::AlertHandler::new(db.clone(), run_config.include_complete_alerts);

    if let Some(api_token) = run_config.alerts_telegram.bot_api_token
        && let Some(chat_id) = run_config.alerts_telegram.chat_id
    {
        info!("adding telegram alert target");

        alert_handler.add_target(Box::new(alerts::TelegramAlertTarget::new(
            api_token, chat_id,
        )));
    }

    let alert_handler = Arc::new(alert_handler);

    let print = Print::new(
        db.clone(),
        weblink_state.clone(),
        ipp_client.clone(),
        run_config.skip,
    );

    let state = Arc::new(web::AppState {
        token: token.clone(),
        db: db.clone(),
        print,
        ipp: ipp_client,
        weblink: weblink_state,
        alert_handler: alert_handler.clone(),
        image_cache: Mutex::new(LruCache::new(run_config.cached_images)),
        client: Default::default(),
    });

    let trace_layer = tower_http::trace::TraceLayer::new_for_http()
        .on_request(DefaultOnRequest::new().level(Level::INFO))
        .on_response(DefaultOnResponse::new().level(Level::INFO));

    let (prom_layer, metrics_handler) = PrometheusMetricLayer::pair();

    let main_app: Router<Arc<web::AppState>> = Router::new()
        .merge(web::ui_routes())
        .nest("/api", web::api_routes())
        .route(
            "/metrics",
            axum::routing::get(|| async move { metrics_handler.render() }),
        );

    if let Some(alerts_addr) = run_config.alerts_address {
        alerts::alert_listener(alert_handler, alerts_addr, token.clone()).await?;
    }

    if let Some(weblink_state) = state.weblink.as_ref() {
        info!("enabling weblink");

        crypto::aws_lc_rs::default_provider()
            .install_default()
            .unwrap();

        let weblink_app: Router<Arc<web::AppState>> = Router::new().route(
            "/weblink",
            axum::routing::get(weblink::endpoint).route_layer(middleware::from_fn_with_state(
                state.clone(),
                weblink::require_printer_cert,
            )),
        );

        let store = weblink::get_root_store(&[&weblink_state.root_ca])?;

        let (server_chain, server_key) =
            weblink::fetch_or_generate_server_cert(&db, &run_config.weblink).await?;

        let client_cert_verifier = WebPkiClientVerifier::builder(Arc::new(store));

        let client_cert_verifier = if run_config.weblink.weblink_address.is_some() {
            client_cert_verifier
        } else {
            client_cert_verifier.allow_unauthenticated()
        }
        .build()?;

        let server_config = ServerConfig::builder()
            .with_client_cert_verifier(client_cert_verifier)
            .with_single_cert(server_chain, server_key)?;

        let config = RustlsConfig::from_config(Arc::new(server_config));

        if let Some(weblink_address) = run_config.weblink.weblink_address {
            info!(
                "listening on {}, weblink on {weblink_address}",
                run_config.address
            );

            let listener = tokio::net::TcpListener::bind(run_config.address).await?;
            let main_fut = axum::serve(
                listener,
                main_app
                    .layer(trace_layer.clone())
                    .layer(prom_layer.clone())
                    .with_state(state.clone())
                    .into_make_service(),
            )
            .with_graceful_shutdown(token.clone().cancelled_owned());

            let weblink_fut = axum_server::bind(weblink_address)
                .acceptor(weblink::AcceptorWithCerts::new(RustlsAcceptor::new(config)))
                .handle(handle)
                .serve(
                    weblink_app
                        .layer(trace_layer)
                        .layer(prom_layer)
                        .with_state(state.clone())
                        .into_make_service(),
                );

            tokio::try_join!(main_fut, weblink_fut)?;
        } else {
            info!("listening on {} with tls", run_config.address);

            axum_server::bind(run_config.address)
                .acceptor(weblink::AcceptorWithCerts::new(RustlsAcceptor::new(config)))
                .handle(handle)
                .serve(
                    main_app
                        .merge(weblink_app)
                        .layer(trace_layer)
                        .layer(prom_layer)
                        .with_state(state.clone())
                        .into_make_service(),
                )
                .await?;
        }
    } else {
        info!("listening on {}", run_config.address);

        let listener = tokio::net::TcpListener::bind(run_config.address).await?;
        axum::serve(
            listener,
            main_app
                .layer(trace_layer)
                .layer(prom_layer)
                .with_state(state.clone())
                .into_make_service(),
        )
        .with_graceful_shutdown(token.clone().cancelled_owned())
        .await?;
    }

    info!("server ended, waiting for tasks to finish");

    token.cancel();

    if let Some(weblink) = state.weblink.as_ref() {
        weblink.tasks.close();
        weblink.tasks.wait().await;
    }

    Ok(())
}

async fn shutdown_signal() -> (axum_server::Handle<SocketAddr>, CancellationToken) {
    let token = CancellationToken::new();
    let handle = axum_server::Handle::<SocketAddr>::new();

    tokio::spawn({
        let token = token.clone();
        let handle = handle.clone();

        async move {
            tokio::signal::ctrl_c().await.unwrap();
            info!("shutting down");
            token.cancel();
            handle.graceful_shutdown(None);
        }
    });

    (handle, token)
}

#[derive(Parser)]
struct Config {
    /// Enable excessively pretty log formatting.
    #[clap(long, env)]
    pretty_logs: bool,

    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Start serving requests.
    Serve(Box<RunConfig>),
    /// Generate a new secret for certificate encryption.
    GenerateSecret,
    /// Generate a new root certificate authority for weblink.
    GenerateCa(weblink::GenerateCaConfig),
}

#[derive(Parser)]
struct RunConfig {
    /// Database DSN.
    #[clap(long, env)]
    database_url: String,
    /// Address where http server is bound.
    #[clap(long, env, default_value = "0.0.0.0:3000")]
    address: SocketAddr,
    /// Address to receive sent printer alerts.
    #[clap(long, env)]
    alerts_address: Option<SocketAddr>,
    /// If "PQ JOB COMPLETED" alerts should be recorded.
    #[clap(long, env)]
    include_complete_alerts: bool,
    /// Maximum number of cached images.
    #[clap(long, env, default_value = "64")]
    cached_images: NonZeroUsize,
    /// Host for CUPS server, if one should be used.
    #[clap(long, env)]
    cups_host: Option<Uri>,
    /// Skip printing labels, for testing.
    #[clap(short, long, env)]
    skip: bool,
    #[command(flatten)]
    alerts_telegram: TelegramConfig,
    #[command(flatten)]
    weblink: weblink::WebLinkConfig,
}

#[derive(Clone, clap::Args)]
struct TelegramConfig {
    /// Telegram API token for sending alerts.
    #[clap(
        long = "telegram-api-token",
        env = "TELEGRAM_API_TOKEN",
        requires = "chat_id"
    )]
    bot_api_token: Option<String>,
    /// Telegram chat ID for sending alerts.
    #[clap(
        long = "telegram-chat-id",
        env = "TELEGRAM_CHAT_ID",
        requires = "bot_api_token"
    )]
    chat_id: Option<i64>,
}

struct AppError(eyre::Error);

impl<E> From<E> for AppError
where
    E: Into<eyre::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}
