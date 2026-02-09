use std::collections::HashMap;
use std::sync::Arc;

use axum::body::Body;
use axum::{
    Json, Router,
    body::Bytes,
    extract::{Path, State},
    http::{HeaderMap, HeaderValue, Method, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, post},
};
use itertools::izip;
use rcgen::SerialNumber;
use rust_decimal::Decimal;
use sea_orm::{
    ColumnTrait, EntityTrait, LoaderTrait, QueryFilter, QueryOrder, prelude::DateTimeWithTimeZone,
};
use serde::{Deserialize, Serialize};
use tower_http::cors::CorsLayer;
use tracing::debug;

use crate::{
    AppError,
    entities::*,
    web::{AppState, PrinterConnection, UrlId},
};

pub fn routes() -> Router<Arc<AppState>> {
    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST])
        .allow_origin(tower_http::cors::Any);

    Router::new()
        .nest(
            "/v1",
            Router::new()
                .route("/printers", get(list))
                .route("/printers/{id}", get(detail))
                .route("/printers/{id}/print", post(print))
                .route("/printers/{id}/send", post(send)),
        )
        .layer(cors)
}

fn json_error(status_code: StatusCode, reason: &str) -> (StatusCode, Response<Body>) {
    (
        status_code,
        Json(serde_json::json!({
            "code": status_code.as_u16(),
            "reason": reason,
        }))
        .into_response(),
    )
}

#[derive(Serialize)]
struct PrinterDetail {
    id: UrlId,
    name: String,
    unique_id: Option<String>,
    dpmm: i16,
    label_size: Option<LabelSizeDetail>,
    web_link_certificates: Vec<WebLinkCertificateDetail>,
    connection_type: Option<PrinterConnectionType>,
}

impl
    From<(
        printer::Model,
        Option<label_size::Model>,
        Vec<web_link_certificate::Model>,
    )> for PrinterDetail
{
    fn from(
        (printer, label_size, certs): (
            printer::Model,
            Option<label_size::Model>,
            Vec<web_link_certificate::Model>,
        ),
    ) -> Self {
        Self {
            connection_type: PrinterConnectionType::best_connection_type(&printer, &certs),
            id: printer.id.into(),
            name: printer.name,
            unique_id: printer.unique_id,
            dpmm: printer.dpmm,
            label_size: label_size.map(Into::into),
            web_link_certificates: certs.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
enum PrinterConnectionType {
    Network,
    Cups,
    Weblink,
}

impl PrinterConnectionType {
    fn best_connection_type(
        printer: &printer::Model,
        certs: &[web_link_certificate::Model],
    ) -> Option<PrinterConnectionType> {
        let last_weblink_ping = certs
            .iter()
            .filter(|cert| cert.revoked_at.is_none())
            .map(|cert| cert.last_ping_at)
            .max()
            .flatten();

        if matches!(
            last_weblink_ping,
            Some(time) if time > chrono::Utc::now() - chrono::Duration::minutes(2)
        ) {
            return Some(PrinterConnectionType::Weblink);
        }

        let connection_details: Option<PrinterConnection> = printer
            .connection
            .clone()
            .map(serde_json::from_value)
            .transpose()
            .ok()
            .flatten();

        match connection_details {
            Some(PrinterConnection::Network { .. }) => Some(PrinterConnectionType::Network),
            Some(PrinterConnection::Cups { .. }) => Some(PrinterConnectionType::Cups),
            _ => None,
        }
    }
}

#[derive(Serialize)]
struct LabelSizeDetail {
    id: UrlId,
    width: Decimal,
    height: Decimal,
}

impl From<label_size::Model> for LabelSizeDetail {
    fn from(value: label_size::Model) -> Self {
        Self {
            id: value.id.into(),
            width: value.width,
            height: value.height,
        }
    }
}

#[derive(Serialize)]
struct WebLinkCertificateDetail {
    id: UrlId,
    serial_number: String,
    created_at: DateTimeWithTimeZone,
    expires_at: DateTimeWithTimeZone,
    revoked_at: Option<DateTimeWithTimeZone>,
    last_ping_at: Option<DateTimeWithTimeZone>,
}

impl From<web_link_certificate::Model> for WebLinkCertificateDetail {
    fn from(value: web_link_certificate::Model) -> Self {
        Self {
            id: value.id.into(),
            serial_number: SerialNumber::from_slice(&value.serial_number).to_string(),
            created_at: value.created_at,
            expires_at: value.expires_at,
            revoked_at: value.revoked_at,
            last_ping_at: value.last_ping_at,
        }
    }
}

async fn list(State(state): State<Arc<AppState>>) -> Result<(StatusCode, Response), AppError> {
    let printers = printer::Entity::find()
        .order_by_asc(printer::Column::Id)
        .all(&state.db)
        .await?;

    let label_sizes = printers.load_one(label_size::Entity, &state.db).await?;

    let certs = printers
        .load_many(
            web_link_certificate::Entity::find()
                .filter(web_link_certificate::Column::ExpiresAt.gt(chrono::Utc::now()))
                .order_by_asc(web_link_certificate::Column::Id),
            &state.db,
        )
        .await?;

    let details: Vec<PrinterDetail> = izip!(printers, label_sizes, certs)
        .map(Into::into)
        .collect();

    Ok((
        StatusCode::OK,
        axum::response::Json(serde_json::json!({
            "printers": details,
        }))
        .into_response(),
    ))
}

#[derive(Deserialize)]
struct PrintPath {
    #[serde(rename = "id")]
    printer_id: UrlId,
}

async fn detail(
    Path(path): Path<PrintPath>,
    State(state): State<Arc<AppState>>,
) -> Result<(StatusCode, Response), AppError> {
    let (printer, label_size) = printer::Entity::find_by_id(path.printer_id)
        .find_also_related(label_size::Entity)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown printer id"))?;

    let certs = web_link_certificate::Entity::find()
        .filter(web_link_certificate::Column::PrinterId.eq(printer.id))
        .order_by_asc(web_link_certificate::Column::Id)
        .all(&state.db)
        .await?;

    let details = PrinterDetail::from((printer, label_size, certs));

    Ok((
        StatusCode::OK,
        axum::response::Json(details).into_response(),
    ))
}

#[derive(Deserialize)]
struct PrintJson {
    label_id: UrlId,
    #[serde(rename = "data")]
    fields: Option<HashMap<String, String>>,
}

async fn print(
    Path(path): Path<PrintPath>,
    State(state): State<Arc<AppState>>,
    Json(data): Json<PrintJson>,
) -> Result<(StatusCode, Response), AppError> {
    let Some(printer) = printer::Entity::find_by_id(path.printer_id)
        .one(&state.db)
        .await?
    else {
        return Ok(json_error(
            StatusCode::NOT_FOUND,
            "Printer could not be found.",
        ));
    };

    let label = label::Entity::find_by_id(data.label_id)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown label"))?;

    let (history, verifications) = state
        .print
        .send_label(
            printer,
            &label.zpl,
            data.fields.unwrap_or_default(),
            Some(label.label_size_id),
            Some(label.id),
        )
        .await?;

    let mut headers = HeaderMap::with_capacity(1);
    headers.insert(
        "x-history-id",
        HeaderValue::from_str(&UrlId(history.id).to_string())?,
    );

    if let Some(verifications) = verifications {
        Ok((
            StatusCode::OK,
            (
                headers,
                Json(serde_json::json!({
                    "verifications": verifications,
                })),
            )
                .into_response(),
        ))
    } else {
        Ok((StatusCode::NO_CONTENT, headers.into_response()))
    }
}

async fn send(
    Path(path): Path<PrintPath>,
    State(state): State<Arc<AppState>>,
    body: Bytes,
) -> Result<(StatusCode, Response), AppError> {
    let Some(printer) = printer::Entity::find_by_id(path.printer_id)
        .one(&state.db)
        .await?
    else {
        return Ok(json_error(
            StatusCode::NOT_FOUND,
            "Printer could not be found.",
        ));
    };

    debug!("got {} bytes", body.len());

    state.print.send_raw(printer, body).await?;

    Ok((StatusCode::NO_CONTENT, Response::default()))
}
