use std::{collections::HashMap, net::SocketAddr, num::NonZeroUsize, sync::Arc};

use axum::{Router, Server};
use axum_prometheus::PrometheusMetricLayer;
use clap::Parser;
use lru::LruCache;
use sea_orm::Database;
use sha2::{Digest, Sha256};
use tokio::{io::AsyncWriteExt, net::TcpStream, sync::Mutex};
use tower_http::trace::{DefaultOnRequest, DefaultOnResponse};
use tracing::{instrument, Level};

use entities::{label, label_size, printer};
use migration::{Migrator, MigratorTrait};

mod entities;
mod template;
mod web;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    if std::env::var_os("RUST_LOG").is_none() {
        std::env::set_var("RUST_LOG", "info,sqlx=warn,tower_http::trace=debug");
    }

    let config = Config::parse();
    tracing_subscriber::fmt::init();

    let db = Database::connect(&config.database_url).await?;
    Migrator::up(&db, None).await?;

    let state = Arc::new(web::AppState {
        db,
        image_cache: Mutex::new(LruCache::new(config.cached_images)),
        client: Default::default(),
    });

    let (prom_layer, metrics_handler) = PrometheusMetricLayer::pair();

    let app = Router::new()
        .merge(web::ui_routes())
        .nest("/api", web::api_routes())
        .route(
            "/metrics",
            axum::routing::get(|| async move { metrics_handler.render() }),
        )
        .with_state(state)
        .layer(
            tower_http::trace::TraceLayer::new_for_http()
                .on_request(DefaultOnRequest::new().level(Level::INFO))
                .on_response(DefaultOnResponse::new().level(Level::INFO)),
        )
        .layer(prom_layer);

    tracing::info!("listening on {}", config.address);
    Server::bind(&config.address)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}

#[derive(Parser)]
struct Config {
    /// Database DSN.
    #[clap(long, env)]
    database_url: String,
    /// Address where http server is bound.
    #[clap(long, env, default_value = "0.0.0.0:3000")]
    address: SocketAddr,
    /// Maximum number of cached images.
    #[clap(long, env, default_value = "64")]
    cached_images: NonZeroUsize,
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

#[instrument(skip(image_cache, client, zpl), fields(key))]
async fn render_zpl(
    image_cache: &Mutex<LruCache<[u8; 32], Vec<u8>>>,
    client: &reqwest::Client,
    zpl: &str,
    label_size: &label_size::Model,
    dpmm: i16,
) -> eyre::Result<Vec<u8>> {
    let mut hasher = Sha256::new();
    hasher.update(dpmm.to_ne_bytes());
    hasher.update(label_size.id.as_bytes());
    hasher.update(zpl.as_bytes());
    let key: [u8; 32] = hasher.finalize().try_into().unwrap();
    tracing::Span::current().record("key", hex::encode(key));

    {
        let mut cache = image_cache.lock().await;

        if let Some(image) = cache.get(&key) {
            tracing::info!("had image cached");
            return Ok(image.to_owned());
        }
    }

    let url = format!(
        "https://api.labelary.com/v1/printers/{dpmm}dpmm/labels/{width}x{height}/0/",
        width = label_size.width,
        height = label_size.height
    );
    tracing::debug!(url, "making request to url");

    let data = client
        .post(url)
        .body(zpl.to_owned())
        .header("accept", "image/png")
        .send()
        .await?
        .bytes()
        .await?;

    image_cache.lock().await.put(key, data.to_vec());
    tracing::debug!("added image to cache");

    Ok(data.to_vec())
}

#[tracing::instrument(skip_all, fields(printer_id = %printer.id, label_id = %label.id))]
async fn send_print_job(
    printer: printer::Model,
    label: label::Model,
    variables: HashMap<String, String>,
) -> eyre::Result<()> {
    if printer.label_size_id.is_some() && printer.label_size_id != Some(label.label_size_id) {
        return Err(eyre::eyre!("label size does not match printer"));
    }

    tracing::debug!("printing on printer with variables: {variables:?}");

    let variables: HashMap<String, String> = variables
        .into_iter()
        .filter(|(_key, value)| !value.is_empty())
        .collect();

    let context = tera::Context::from_serialize(variables)?;
    let data = template::render_label(&label.zpl, &context)?;

    tracing::trace!("printing data: {data}");

    let mut conn = TcpStream::connect(printer.address).await?;
    conn.write_all(data.as_bytes()).await?;
    conn.shutdown().await?;

    tracing::info!("finished print job");

    axum_prometheus::metrics::increment_counter!("zpl_printer_print_total");

    Ok(())
}
