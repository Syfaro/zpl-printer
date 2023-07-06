use std::collections::HashMap;
use std::sync::Arc;

use axum::{
    extract::State,
    http::{HeaderMap, HeaderValue, Method, StatusCode},
    routing::post,
    Json, Router,
};
use sea_orm::EntityTrait;
use serde::Deserialize;
use tower_http::cors::CorsLayer;

use crate::{
    entities::*,
    send_print_job,
    web::{AppState, UrlId},
    AppError,
};

pub fn routes() -> Router<Arc<AppState>> {
    let cors = CorsLayer::new()
        .allow_methods([Method::POST])
        .allow_origin(tower_http::cors::Any);

    Router::new()
        .nest("/v1", Router::new().route("/print", post(print)))
        .layer(cors)
}

#[derive(Deserialize)]
struct PrintJson {
    printer_id: UrlId,
    label_id: UrlId,
    #[serde(rename = "data")]
    fields: HashMap<String, String>,
}

async fn print(
    State(state): State<Arc<AppState>>,
    Json(data): Json<PrintJson>,
) -> Result<(StatusCode, HeaderMap), AppError> {
    let printer = printer::Entity::find_by_id(data.printer_id)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown printer"))?;

    let label = label::Entity::find_by_id(data.label_id)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown label"))?;

    let id = send_print_job(&state.db, printer, label, data.fields, state.skip, true).await?;

    let mut headers = HeaderMap::with_capacity(1);
    headers.insert("x-history-id", HeaderValue::from_str(&id.to_string())?);

    Ok((StatusCode::NO_CONTENT, headers))
}
