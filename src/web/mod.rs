mod api;
mod ui;

pub use api::routes as api_routes;
use askama_axum::IntoResponse;
use async_trait::async_trait;
use axum::{
    extract::FromRequestParts,
    http::{request::Parts, StatusCode},
};
use base64::Engine;
use lru::LruCache;
use sea_orm::DatabaseConnection;
use serde::Deserialize;
use tokio::sync::Mutex;
pub use ui::routes as ui_routes;
use uuid::Uuid;

use crate::AppError;

pub struct AppState {
    pub db: DatabaseConnection,
    pub image_cache: Mutex<LruCache<[u8; 32], Vec<u8>>>,
    pub client: reqwest::Client,
}

#[derive(Debug, Default, PartialEq)]
#[allow(dead_code)]
enum RequestType {
    #[default]
    Normal,
    Htmx {
        current_url: Option<String>,
        prompt: Option<String>,
        target: Option<String>,
        trigger_name: Option<String>,
    },
}

impl RequestType {
    fn is_htmx(&self) -> bool {
        matches!(self, RequestType::Htmx { .. })
    }
}

#[async_trait]
impl<S> FromRequestParts<S> for RequestType
where
    S: Send + Sync,
{
    type Rejection = (StatusCode, &'static str);

    async fn from_request_parts(parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        let get_header = |name: &str| -> Result<Option<String>, Self::Rejection> {
            parts
                .headers
                .get(name)
                .map(|val| val.to_str().map(|val| val.to_string()))
                .transpose()
                .map_err(|_err| (StatusCode::BAD_REQUEST, "invalid header"))
        };

        if get_header("hx-request")?.as_deref() == Some("true") {
            Ok(Self::Htmx {
                current_url: get_header("hx-current-url")?,
                prompt: get_header("hx-prompt")?,
                target: get_header("hx-target")?,
                trigger_name: get_header("hx-trigger-name")?,
            })
        } else {
            Ok(Self::Normal)
        }
    }
}

impl IntoResponse for AppError {
    fn into_response(self) -> askama_axum::Response {
        (StatusCode::INTERNAL_SERVER_ERROR, self.0.to_string()).into_response()
    }
}

pub trait AsUrl {
    fn as_url(&self) -> String;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct UrlId(Uuid);

impl AsUrl for UrlId {
    fn as_url(&self) -> String {
        self.to_string()
    }
}

impl AsUrl for Uuid {
    fn as_url(&self) -> String {
        UrlId(*self).to_string()
    }
}

impl From<Uuid> for UrlId {
    fn from(value: Uuid) -> Self {
        Self(value)
    }
}

impl From<UrlId> for Uuid {
    fn from(value: UrlId) -> Self {
        value.0
    }
}

impl std::fmt::Display for UrlId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(self.0)
        )
    }
}

impl std::str::FromStr for UrlId {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let data = base64::engine::general_purpose::URL_SAFE_NO_PAD
            .decode(s)
            .map_err(|_err| "not base64")?;
        let data: [u8; 16] = data.try_into().map_err(|_err| "not 16 bytes")?;

        Ok(Self(Uuid::from_bytes(data)))
    }
}

impl<'de> Deserialize<'de> for UrlId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let input: &str = Deserialize::deserialize(deserializer)?;

        let data = base64::engine::general_purpose::URL_SAFE_NO_PAD
            .decode(input)
            .map_err(serde::de::Error::custom)?;

        let data: [u8; 16] = data
            .try_into()
            .map_err(|_err| serde::de::Error::custom("incorrectly sized base64 data"))?;

        Ok(Self(Uuid::from_bytes(data)))
    }
}
