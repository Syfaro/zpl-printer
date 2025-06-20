use std::{collections::HashMap, net::SocketAddr, num::NonZeroUsize, sync::Arc, time::Duration};

use axum::{Router, Server};
use axum_prometheus::PrometheusMetricLayer;
use clap::Parser;
use lru::LruCache;
use sea_orm::{
    ActiveValue::NotSet, ColumnTrait, Database, DatabaseConnection, EntityTrait, QueryFilter, Set,
};
use sha2::{Digest, Sha256};
use tap::TapFallible;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
    sync::Mutex,
};
use tower_http::trace::{DefaultOnRequest, DefaultOnResponse};
use tracing::{Level, instrument};

use entities::{label, label_size, printer};
use migration::{Migrator, MigratorTrait, SimpleExpr};
use uuid::Uuid;

use crate::entities::history;

mod entities;
mod template;
mod web;
mod zpl;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    if std::env::var_os("RUST_LOG").is_none() {
        unsafe { std::env::set_var("RUST_LOG", "info,sqlx=warn,tower_http::trace=debug") }
    }

    let config = Config::parse();
    tracing_subscriber::fmt::init();

    let db = Database::connect(&config.database_url).await?;
    Migrator::up(&db, None).await?;

    let state = Arc::new(web::AppState {
        db,
        image_cache: Mutex::new(LruCache::new(config.cached_images)),
        client: Default::default(),
        skip: config.skip,
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
    /// Skip printing labels, for testing.
    #[clap(short, long, env)]
    skip: bool,
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
    let key: [u8; 32] = hasher.finalize().into();
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

type LabelValidations = Vec<HashMap<u16, String>>;

#[tracing::instrument(skip_all, fields(printer_id = %printer.id, label_id = %label.id, skip))]
async fn send_print_job(
    db: &DatabaseConnection,
    printer: printer::Model,
    label: label::Model,
    variables: HashMap<String, String>,
    skip: bool,
    real_label: bool,
) -> eyre::Result<(Uuid, Option<LabelValidations>)> {
    if printer.label_size_id.is_some() && printer.label_size_id != Some(label.label_size_id) {
        return Err(eyre::eyre!("label size does not match printer"));
    }

    tracing::debug!("printing on printer with variables: {variables:?}");

    let variables: HashMap<String, String> = variables
        .into_iter()
        .filter(|(_key, value)| !value.is_empty())
        .collect();

    let label_id = if real_label { Some(label.id) } else { None };

    let context = tera::Context::from_serialize(variables.clone())?;
    let data = template::render_label(&label.zpl, &context)?;

    let commands = zpl::ZplParser::parse(&data)
        .tap_err(|err| tracing::error!("could not parse zpl: {err}"))
        .unwrap_or_default();
    let host_verifications = zpl::ZplHostVerification::from_commands(&commands);
    let host_verification_all_labels = host_verifications
        .iter()
        .all(|(_, verification)| verification.applies_to == Some('L'));
    let print_quantity = commands
        .iter()
        .find_map(|command| {
            if command.start == '^' && command.name == "PQ" {
                command.args.parse().ok()
            } else {
                None
            }
        })
        .unwrap_or(1);
    tracing::trace!(
        print_quantity,
        has_host_verifications = !host_verifications.is_empty(),
        host_verification_all_labels,
        "finished parsing zpl"
    );

    let history = history::ActiveModel {
        id: Set(Uuid::now_v7()),
        printer_id: Set(Some(printer.id)),
        label_id: Set(label_id),
        zpl: Set(data.clone()),
        variables: Set(serde_json::to_value(variables)?),
        printed_at: NotSet,
        count: Set(print_quantity),
        verification: NotSet,
    };

    let history_id = history::Entity::insert(history)
        .exec(db)
        .await?
        .last_insert_id;

    tracing::trace!("printing data: {data}");

    let verifications = if skip {
        tracing::warn!("skipping print");
        None
    } else {
        let mut conn = TcpStream::connect(printer.address).await?;
        conn.write_all(data.as_bytes()).await?;

        let verifications = if !host_verifications.is_empty() && host_verification_all_labels {
            match check_host_verifications(
                &mut conn,
                db,
                history_id,
                host_verifications,
                print_quantity,
            )
            .await
            {
                Ok(verifications) => Some(verifications),
                Err(err) => {
                    tracing::error!("could not get verifications: {err}");
                    None
                }
            }
        } else {
            None
        };

        conn.shutdown().await?;
        verifications
    };

    tracing::info!("finished print job");

    axum_prometheus::metrics::increment_counter!("zpl_printer_print_total");

    Ok((history_id, verifications))
}

async fn check_host_verifications(
    conn: &mut TcpStream,
    db: &DatabaseConnection,
    history_id: Uuid,
    host_verifications: HashMap<u16, zpl::ZplHostVerification<'_>>,
    print_quantity: i32,
) -> eyre::Result<LabelValidations> {
    let expected_bytes_per_print: usize = host_verifications
        .values()
        .map(|verification| {
            verification.length
                + verification.prefix.map(|p| p.len()).unwrap_or_default()
                + verification.suffix.map(|s| s.len()).unwrap_or_default()
        })
        .sum();
    tracing::trace!(
        print_quantity,
        expected_bytes_per_print,
        "calculated expected response size"
    );

    let mut buf = vec![0; expected_bytes_per_print * print_quantity as usize];
    tokio::time::timeout(
        Duration::from_secs(3 * print_quantity as u64),
        conn.read_exact(&mut buf),
    )
    .await??;
    tracing::trace!("got response data: {}", String::from_utf8_lossy(&buf));

    let mut pos = buf.as_slice();
    let mut verifications = Vec::new();

    for _ in 0..print_quantity {
        let mut entry = HashMap::with_capacity(host_verifications.len());

        for (id, verification) in host_verifications.iter() {
            if let Some(prefix) = verification.prefix {
                let actual_prefix = &pos[..prefix.len()];
                if actual_prefix != prefix.as_bytes() {
                    eyre::bail!(
                        "host verification had wrong prefix, expected {prefix} but got {}",
                        String::from_utf8_lossy(actual_prefix)
                    );
                }
                pos = &pos[prefix.len()..];
            }

            let data = &pos[..verification.length];
            pos = &pos[verification.length..];

            if let Some(suffix) = verification.suffix {
                let actual_suffix = &pos[..suffix.len()];
                if actual_suffix != suffix.as_bytes() {
                    eyre::bail!(
                        "host verification had wrong suffix, expected {suffix} but got {}",
                        String::from_utf8_lossy(actual_suffix)
                    );
                }
                pos = &pos[suffix.len()..];
            }

            entry.insert(*id, String::from_utf8_lossy(data).to_string());
        }

        verifications.push(entry);
    }

    tracing::debug!("got all verifications: {verifications:#?}");

    history::Entity::update_many()
        .col_expr(
            history::Column::Verification,
            SimpleExpr::Value(serde_json::to_value(&verifications)?.into()),
        )
        .filter(history::Column::Id.eq(history_id))
        .exec(db)
        .await?;

    Ok(verifications)
}
