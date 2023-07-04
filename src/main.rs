use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    error::Error,
    net::SocketAddr,
    num::NonZeroUsize,
    sync::Arc,
};

use askama::Template;
use askama_axum::IntoResponse;
use async_trait::async_trait;
use axum::{
    extract::{FromRequestParts, Path, State},
    http::{request::Parts, HeaderValue, StatusCode},
    response::{Redirect, Response},
    routing, Form, Json, Router,
};
use base64::Engine;
use lru::LruCache;
use reqwest::Method;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use sea_orm::{
    ActiveModelTrait, ActiveValue::NotSet, Database, DatabaseConnection, EntityTrait, QueryOrder,
    Set,
};
use serde::Deserialize;
use serde_with::{serde_as, NoneAsEmptyString};
use sha2::{Digest, Sha256};
use tera::{
    ast::{Expr, ExprVal, FunctionCall, In, LogicExpr, MacroCall, MathExpr, Node},
    Context, Tera,
};
use tokio::{io::AsyncWriteExt, net::TcpStream, sync::Mutex};
use tracing::instrument;

use entities::{label, label_size, printer};
use migration::{Migrator, MigratorTrait};
use uuid::Uuid;

mod entities;

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

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt::init();

    let database_url = std::env::var("DATABASE_URL")?;
    let db = Database::connect(database_url).await?;

    Migrator::up(&db, None).await?;

    let state = Arc::new(AppState {
        client: Default::default(),
        image_cache: Mutex::new(LruCache::new(NonZeroUsize::new(64).unwrap())),
        db,
    });

    let app = Router::new()
        .route("/", routing::get(|| async { Redirect::to("/labels") }))
        .route("/labels", routing::get(labels).post(label))
        .route("/labels/:id", routing::put(label).delete(label))
        .route("/label_sizes", routing::post(label_sizes))
        .route("/label_sizes/:id", routing::delete(label_size))
        .route("/printers", routing::post(printers))
        .route("/printers/:id", routing::get(printer).put(printer))
        .route("/playground", routing::get(playground).post(playground))
        .route("/playground/:id", routing::get(playground))
        .route("/playground/print/:id", routing::post(playground_print))
        .nest(
            "/api",
            Router::new().nest("/v1", Router::new().route("/print", routing::post(print))),
        )
        .with_state(state);

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}

struct AppState {
    client: reqwest::Client,
    image_cache: Mutex<LruCache<[u8; 32], Vec<u8>>>,
    db: DatabaseConnection,
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

struct AppError(eyre::Error);

impl IntoResponse for AppError {
    fn into_response(self) -> askama_axum::Response {
        (StatusCode::INTERNAL_SERVER_ERROR, self.0.to_string()).into_response()
    }
}

impl<E> From<E> for AppError
where
    E: Into<eyre::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}

#[derive(Template, Default)]
#[template(path = "labels/index.html")]
struct LabelTemplate {
    labels: Vec<(label::Model, label_size::Model)>,
    label_sizes: Vec<label_size::Model>,
    printers: Vec<(printer::Model, Option<label_size::Model>)>,
}

async fn labels(State(state): State<Arc<AppState>>) -> Result<Response, AppError> {
    let labels = label::Entity::find()
        .order_by_asc(label::Column::Name)
        .find_also_related(label_size::Entity)
        .all(&state.db)
        .await?
        .into_iter()
        .filter_map(|(label, label_size)| label_size.map(|label_size| (label, label_size)))
        .collect();

    let label_sizes = label_size::Entity::find()
        .order_by_asc(label_size::Column::Width)
        .order_by_asc(label_size::Column::Height)
        .all(&state.db)
        .await?;

    let printers = printer::Entity::find()
        .order_by_asc(printer::Column::Name)
        .find_also_related(label_size::Entity)
        .all(&state.db)
        .await?;

    Ok(LabelTemplate {
        labels,
        label_sizes,
        printers,
    }
    .into_response())
}

async fn label(
    State(state): State<Arc<AppState>>,
    request_type: RequestType,
    method: Method,
    path_id: Option<Path<UrlId>>,
    form: Option<Form<PlaygroundForm>>,
) -> Result<Response, AppError> {
    let Form(form) = form.unwrap_or_default();

    if method == Method::DELETE {
        let id = path_id
            .map(|id| id.0)
            .or(form.id)
            .ok_or_else(|| eyre::eyre!("missing id"))?;

        label::Entity::delete_by_id(id).exec(&state.db).await?;

        return hx_load(&request_type, false, "/labels");
    }

    let empty_model = || {
        (
            true,
            label::ActiveModel {
                id: Set(Uuid::now_v7()),
                ..Default::default()
            },
        )
    };

    let (is_new, mut label) = if let Some(id) = form.id {
        let label = label::Entity::find_by_id(id).one(&state.db).await?;

        label
            .map(|label| (false, label.into()))
            .unwrap_or_else(empty_model)
    } else {
        empty_model()
    };

    match &request_type {
        RequestType::Htmx {
            prompt: Some(prompt),
            ..
        } if is_new => {
            label.name = Set(prompt.to_owned());
        }
        _ => (),
    }

    label.zpl = Set(form.zpl);
    label.dpmm = Set(form.dpmm);
    label.label_size_id = match form.label_size_id {
        Some(id) => Set(id.into()),
        None => return Err(eyre::eyre!("missing label size id").into()),
    };

    if is_new {
        label.insert(&state.db).await?;
    } else {
        label.update(&state.db).await?;
    }

    hx_load(&request_type, false, "/labels")
}

#[derive(Deserialize)]
struct LabelSizeForm {
    width: Decimal,
    height: Decimal,
}

async fn label_sizes(
    State(state): State<Arc<AppState>>,
    Form(form): Form<LabelSizeForm>,
) -> Result<Response, AppError> {
    let label_size = label_size::ActiveModel {
        id: Set(Uuid::now_v7()),
        width: Set(form.width),
        height: Set(form.height),
    };

    let label_size = label_size::Entity::insert(label_size)
        .exec(&state.db)
        .await?;
    tracing::info!(id = %label_size.last_insert_id, "inserted new label size");

    Ok(Redirect::to("/labels").into_response())
}

fn hx_load(request_type: &RequestType, reload: bool, fallback: &str) -> Result<Response, AppError> {
    match request_type {
        RequestType::Normal => Ok(Redirect::to(fallback).into_response()),
        RequestType::Htmx { current_url, .. } => {
            let mut resp = StatusCode::NO_CONTENT.into_response();

            let location = if reload {
                current_url.as_deref().unwrap_or(fallback)
            } else {
                fallback
            };

            resp.headers_mut()
                .insert("hx-location", HeaderValue::from_str(location)?);

            Ok(resp)
        }
    }
}

async fn label_size(
    State(state): State<Arc<AppState>>,
    method: Method,
    request_type: RequestType,
    Path(id): Path<UrlId>,
) -> Result<Response, AppError> {
    match method {
        Method::DELETE => {
            label_size::Entity::delete_by_id(id).exec(&state.db).await?;
        }
        _ => return Err(eyre::eyre!("unknown method").into()),
    }

    hx_load(&request_type, true, "/labels")
}

#[serde_as]
#[derive(Deserialize)]
struct PrinterForm {
    name: String,
    address: SocketAddr,
    dpmm: i16,
    #[serde(default)]
    #[serde_as(as = "NoneAsEmptyString")]
    current_size: Option<UrlId>,
}

async fn printers(
    State(state): State<Arc<AppState>>,
    request_type: RequestType,
    Form(form): Form<PrinterForm>,
) -> Result<Response, AppError> {
    let label_size_id = if let Some(current_size) = form.current_size {
        Set(label_size::Entity::find_by_id(current_size)
            .one(&state.db)
            .await
            .ok()
            .flatten()
            .map(|label_size| label_size.id))
    } else {
        NotSet
    };

    let printer = printer::ActiveModel {
        id: Set(Uuid::now_v7()),
        name: Set(form.name),
        address: Set(form.address.to_string()),
        dpmm: Set(form.dpmm),
        label_size_id,
    };

    printer.insert(&state.db).await?;

    hx_load(&request_type, true, "/labels")
}

#[derive(Template)]
#[template(path = "labels/printer.html")]
struct PrinterTemplate {
    printer: printer::Model,
    current_size: Option<label_size::Model>,
    label_sizes: Vec<label_size::Model>,
}

impl PrinterTemplate {
    fn is_current_size(&self, size: &label_size::Model) -> bool {
        self.current_size.as_ref() == Some(size)
    }
}

async fn printer(
    State(state): State<Arc<AppState>>,
    method: Method,
    request_type: RequestType,
    Path(id): Path<UrlId>,
    form: Option<Form<PrinterForm>>,
) -> Result<Response, AppError> {
    let (printer, current_size) = printer::Entity::find_by_id(id)
        .find_also_related(label_size::Entity)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown printer"))?;

    let label_sizes = label_size::Entity::find()
        .order_by_asc(label_size::Column::Width)
        .order_by_asc(label_size::Column::Height)
        .all(&state.db)
        .await?;

    match method {
        Method::GET => Ok(PrinterTemplate {
            printer,
            current_size,
            label_sizes,
        }
        .into_response()),
        Method::PUT => {
            let Form(form) = form.ok_or_else(|| eyre::eyre!("missing form"))?;

            let mut printer: printer::ActiveModel = printer.into();
            printer.name = Set(form.name);
            printer.address = Set(form.address.to_string());
            printer.dpmm = Set(form.dpmm);
            printer.label_size_id = if let Some(current_size) = form.current_size {
                Set(Some(current_size.into()))
            } else {
                Set(None)
            };

            printer.update(&state.db).await?;

            hx_load(&request_type, false, "/labels")
        }
        _ => Err(eyre::eyre!("unknown method").into()),
    }
}

#[derive(Template, Default)]
#[template(path = "zpl/fields.html")]
struct ZplFieldsTemplate {
    fields: BTreeMap<String, String>,
}

#[derive(Template)]
#[template(path = "playground/index.html")]
struct PlaygroundTemplate<'a> {
    zpl: &'a str,
    fields: ZplFieldsTemplate,

    rendered: Result<String, String>,
    preview: Option<String>,

    label_sizes: Vec<label_size::Model>,
    printers: Vec<printer::Model>,

    label: Option<label::Model>,
}

impl PlaygroundTemplate<'_> {
    fn selected_dpmm(&self) -> i16 {
        match &self.label {
            Some(label) => label.dpmm,
            None => 8,
        }
    }

    fn selected_size(&self) -> Uuid {
        match &self.label {
            Some(label) => label.label_size_id,
            None => match self.label_sizes.first() {
                Some(label_size) => label_size.id,
                None => Uuid::now_v7(),
            },
        }
    }
}

#[derive(Template)]
#[template(path = "playground/update.html")]
struct PlaygroundUpdateTemplate {
    fields: ZplFieldsTemplate,
    rendered: Result<String, String>,
    preview: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
struct PlaygroundForm {
    #[serde(rename = "_id")]
    id: Option<UrlId>,
    #[serde(rename = "_label_size_id")]
    label_size_id: Option<UrlId>,
    #[serde(rename = "_dpmm")]
    dpmm: i16,
    #[serde(rename = "_zpl")]
    zpl: String,
    #[serde(flatten)]
    fields: HashMap<String, String>,
}

fn get_template_error(err: tera::Error) -> String {
    match err
        .source()
        .and_then(|err| err.downcast_ref::<tera::Error>())
    {
        Some(tera_error) => tera_error.to_string(),
        None => err.to_string(),
    }
}

fn encode_image_html(data: &[u8]) -> String {
    format!(
        "data:image/png;base64,{}",
        base64::engine::general_purpose::STANDARD.encode(data)
    )
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

async fn playground(
    State(state): State<Arc<AppState>>,
    path_id: Option<Path<UrlId>>,
    request_type: RequestType,
    form: Option<Form<PlaygroundForm>>,
) -> Result<Response, AppError> {
    let Form(form) = form.unwrap_or_default();
    tracing::debug!("got form: {form:?}");

    let PlaygroundForm {
        id: form_id,
        label_size_id,
        dpmm,
        zpl,
        fields,
    } = form;

    let dpmm = if dpmm == 0 { 8 } else { dpmm };

    let label = if let Some(id) = path_id.map(|id| id.0).or(form_id) {
        label::Entity::find_by_id(id).one(&state.db).await?
    } else {
        None
    };

    let zpl = match &label {
        Some(label) if zpl.is_empty() => label.zpl.clone(),
        _ => zpl,
    };

    let label_sizes = label_size::Entity::find()
        .order_by_asc(label_size::Column::Width)
        .order_by_asc(label_size::Column::Height)
        .all(&state.db)
        .await?;

    let printers = printer::Entity::find()
        .order_by_asc(printer::Column::Name)
        .all(&state.db)
        .await?;

    let label_size_id =
        label_size_id.or_else(|| label.as_ref().map(|label| label.label_size_id.into()));
    let label_size = label_sizes
        .iter()
        .find(|label_size| Some(&label_size.id.into()) == label_size_id.as_ref())
        .cloned()
        .unwrap_or_else(|| {
            label_sizes
                .first()
                .cloned()
                .unwrap_or_else(|| label_size::Model {
                    id: Uuid::now_v7(),
                    width: dec!(4),
                    height: dec!(6),
                })
        });

    let variables = match Extractor::extract(&zpl) {
        Ok(variables) => variables,
        Err(_err) if request_type.is_htmx() => return Ok(StatusCode::NO_CONTENT.into_response()),
        Err(err) => {
            return Ok(PlaygroundTemplate {
                zpl: &zpl,
                fields: ZplFieldsTemplate::default(),
                rendered: Err(get_template_error(err)),
                preview: None,
                label_sizes,
                printers,
                label,
            }
            .into_response())
        }
    };

    if variables.iter().any(|variable| variable.starts_with('_')) {
        return Err(eyre::eyre!("variables cannot start with underscore").into());
    }

    let fields: BTreeMap<String, String> = variables
        .iter()
        .map(|var| (var.clone(), fields.get(var).cloned().unwrap_or_default()))
        .collect();

    let mut context = tera::Context::default();
    for (key, val) in &fields {
        if !val.is_empty() {
            context.insert(key, &val);
        }
    }
    let rendered = match render_template(&zpl, &context) {
        Ok(rendered) => Ok(rendered),
        Err(err) => Err(get_template_error(err)),
    };

    let fields = ZplFieldsTemplate { fields };

    let image = if let Ok(zpl) = &rendered {
        render_zpl(&state.image_cache, &state.client, zpl, &label_size, dpmm)
            .await
            .ok()
            .as_deref()
            .map(encode_image_html)
    } else {
        None
    };

    if request_type.is_htmx() {
        return Ok(PlaygroundUpdateTemplate {
            fields,
            rendered,
            preview: image,
        }
        .into_response());
    }

    Ok(PlaygroundTemplate {
        zpl: &zpl,
        fields,
        rendered,
        preview: image,
        label_sizes,
        printers,
        label,
    }
    .into_response())
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
) -> Result<StatusCode, AppError> {
    let printer = printer::Entity::find_by_id(data.printer_id)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown printer"))?;

    let label = label::Entity::find_by_id(data.label_id)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown label"))?;

    send_print_job(printer, label, data.fields).await?;

    Ok(StatusCode::NO_CONTENT)
}

async fn playground_print(
    State(state): State<Arc<AppState>>,
    Path(id): Path<UrlId>,
    Form(form): Form<PlaygroundForm>,
) -> Result<StatusCode, AppError> {
    let printer = printer::Entity::find_by_id(id)
        .one(&state.db)
        .await?
        .ok_or_else(|| eyre::eyre!("unknown printer"))?;

    send_print_job(
        printer,
        label::Model {
            id: form.id.map(Into::into).unwrap_or_else(Uuid::now_v7),
            name: "".to_string(),
            label_size_id: form
                .label_size_id
                .map(Into::into)
                .unwrap_or_else(Uuid::now_v7),
            dpmm: form.dpmm,
            zpl: form.zpl,
        },
        form.fields,
    )
    .await?;

    Ok(StatusCode::NO_CONTENT)
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
    let data = render_template(&label.zpl, &context)?;

    tracing::trace!("printing data: {data}");

    let mut conn = TcpStream::connect(printer.address).await?;
    conn.write_all(data.as_bytes()).await?;
    conn.shutdown().await?;

    tracing::info!("finished print job");

    Ok(())
}

fn escape_fn(input: &str) -> String {
    input
        .replace('_', "_5f")
        .replace('^', "_5e")
        .replace('º', "_f8")
}

fn get_tera() -> Tera {
    let mut tera = Tera::default();

    tera.autoescape_on(vec![".zpl"]);
    tera.set_escape_fn(escape_fn);

    tera
}

fn render_template(content: &str, context: &Context) -> tera::Result<String> {
    let mut tera = get_tera();
    tera.add_raw_template("label.zpl", content)?;

    tera.render("label.zpl", context)
}

#[derive(Default)]
struct Extractor {
    variables: BTreeSet<String>,
    exclusions: HashSet<String>,
}

impl Extractor {
    fn extract(content: &str) -> tera::Result<BTreeSet<String>> {
        let mut tera = get_tera();
        tera.add_raw_template("label.zpl", content)?;

        let mut extractor = Extractor::default();

        for template in tera.templates.into_values() {
            extractor.expand_node(template.ast);
        }

        Ok(extractor.variables)
    }

    fn expand_node(&mut self, nodes: impl IntoIterator<Item = Node>) {
        for node in nodes {
            match node {
                Node::VariableBlock(_, expr) => self.expand_expr(expr),
                Node::MacroDefinition(_, block, _) => self.expand_node(block.body),
                Node::Set(_, set) => {
                    let _span = tracing::trace_span!("set node", key = set.key).entered();

                    if self.variables.contains(&set.key) {
                        tracing::trace!("variable was already known");
                    } else {
                        tracing::trace!("variables did not contain key, adding to exclusions");
                        self.exclusions.insert(set.key);
                    }
                }
                Node::FilterSection(_, block, _) => self.expand_node(block.body),
                Node::Forloop(_, block, _) => self.expand_node(block.body),
                Node::If(stmt, _) => stmt.conditions.into_iter().for_each(|(_, expr, nodes)| {
                    self.expand_expr(expr);
                    self.expand_node(nodes);
                }),
                Node::Block(_, block, _) => self.expand_node(block.body),
                _ => (),
            }
        }
    }

    fn expand_expr(&mut self, expr: Expr) {
        self.expand_expr_val(expr.val)
    }

    fn expand_expr_val(&mut self, expr_val: ExprVal) {
        match expr_val {
            ExprVal::Ident(name) => {
                let _span = tracing::trace_span!("expr ident", name).entered();

                if self.exclusions.contains(&name) {
                    tracing::trace!("name was in exclusions");
                } else {
                    tracing::trace!("name was not in exclusions");
                    self.variables.insert(name);
                }
            }
            ExprVal::Math(MathExpr { lhs, rhs, .. }) => {
                self.expand_expr(*lhs);
                self.expand_expr(*rhs);
            }
            ExprVal::Logic(LogicExpr { lhs, rhs, .. }) => {
                self.expand_expr(*lhs);
                self.expand_expr(*rhs);
            }
            ExprVal::MacroCall(MacroCall { args, .. }) => {
                args.into_values().for_each(|value| self.expand_expr(value))
            }
            ExprVal::FunctionCall(FunctionCall { args, .. }) => {
                args.into_values().for_each(|value| self.expand_expr(value))
            }
            ExprVal::Array(exprs) => exprs.into_iter().for_each(|value| self.expand_expr(value)),
            ExprVal::StringConcat(concat) => concat
                .values
                .into_iter()
                .for_each(|value| self.expand_expr_val(value)),
            ExprVal::In(In { lhs, rhs, .. }) => {
                self.expand_expr(*lhs);
                self.expand_expr(*rhs);
            }
            _ => (),
        }
    }
}
