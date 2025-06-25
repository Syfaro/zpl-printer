use std::{
    collections::{BTreeMap, HashMap},
    error::Error,
    net::SocketAddr,
    sync::Arc,
};

use askama::Template;
use axum::{
    Form, Router,
    extract::{
        Multipart, Path, Query, State,
        multipart::{Field, MultipartRejection},
        rejection::FormRejection,
    },
    http::{Method, StatusCode},
    response::{IntoResponse, Redirect, Response},
    routing::{delete, get, post, put},
};
use base64::Engine;
use image::{EncodableLayout, ImageEncoder, imageops::FilterType::Lanczos3};
use ipp::prelude::{AsyncIppClient, IppOperationBuilder};
use itertools::{Itertools, izip};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use sea_orm::{
    ActiveModelTrait, ActiveValue::NotSet, DbBackend, EntityTrait, FromQueryResult, LoaderTrait,
    QueryOrder, Set, Statement,
};
use serde::{Deserialize, Serialize};
use serde_with::{NoneAsEmptyString, serde_as};
use tap::TapFallible;
use tokio::try_join;
use uuid::Uuid;

use crate::{
    AppError,
    entities::*,
    render_zpl, send_print_job,
    template::{VariableExtractor, render_label},
    web::{AppState, AsUrl, RequestType, UrlId, hx_load},
    zpl,
};

pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/", get(|| async { Redirect::to("/labels") }))
        .route("/labels", get(labels).post(label))
        .route("/labels/{id}", put(label).delete(label))
        .route("/label_sizes", post(label_sizes))
        .route("/label_sizes/{id}", delete(label_size))
        .route("/printers", get(printers).post(printers))
        .route("/printers/{id}", get(printer).put(printer))
        .route("/playground", get(playground).post(playground))
        .route("/playground/{id}", get(playground))
        .route("/playground/print/{id}", post(playground_print))
        .route("/history", get(history))
        .route("/alerts", get(alerts))
        .route("/images", get(images).post(images))
}

fn into_response<T: Template>(t: &T) -> Response {
    match t.render() {
        Ok(body) => {
            let headers = [(
                axum::http::header::CONTENT_TYPE,
                axum::http::HeaderValue::from_static("text/html"),
            )];

            (headers, body).into_response()
        }
        Err(_) => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
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

    Ok(into_response(&LabelTemplate {
        labels,
        label_sizes,
        printers,
    }))
}

async fn label(
    State(state): State<Arc<AppState>>,
    request_type: RequestType,
    method: Method,
    path_id: Option<Path<UrlId>>,
    form: Result<Form<PlaygroundForm>, FormRejection>,
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

#[derive(Template)]
#[template(path = "printers/form.html")]
struct PrinterFormTemplate {
    cups_devices: Option<Vec<(String, String)>>,
    current_size: Option<label_size::Model>,
    label_sizes: Vec<label_size::Model>,

    unique_id: String,
    name: String,
    dpmm: i16,
    connection: Option<PrinterConnection>,
}

impl PrinterFormTemplate {
    fn is_current_size(&self, size: &label_size::Model) -> bool {
        self.current_size.as_ref() == Some(size)
    }

    fn address(&self) -> String {
        match self.connection {
            Some(PrinterConnection::Network { address }) => address.to_string(),
            _ => "".to_string(),
        }
    }

    fn cups_id_selected(&self, id: &str) -> bool {
        matches!(self.connection.as_ref(), Some(PrinterConnection::Cups { uri }) if uri == id)
    }

    pub fn is_network(&self) -> bool {
        matches!(self.connection, Some(PrinterConnection::Network { .. }))
    }

    pub fn is_cups(&self) -> bool {
        matches!(self.connection, Some(PrinterConnection::Cups { .. }))
    }
}

#[derive(Template)]
#[template(path = "printers/add.html")]
struct AddPrinterTemplate {
    form: PrinterFormTemplate,
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct PrinterForm {
    name: String,
    #[serde(default)]
    #[serde_as(as = "NoneAsEmptyString")]
    unique_id: Option<String>,
    #[serde(flatten)]
    connection: Option<PrinterConnection>,
    dpmm: i16,
    #[serde(default)]
    #[serde_as(as = "NoneAsEmptyString")]
    current_size: Option<UrlId>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "connection_type", rename_all = "snake_case")]
pub enum PrinterConnection {
    Network { address: SocketAddr },
    Cups { uri: String },
}

async fn cups_devices(ipp: Option<&AsyncIppClient>) -> eyre::Result<Option<Vec<(String, String)>>> {
    let devices = if let Some(ipp) = ipp.as_ref() {
        let resp = ipp.send(IppOperationBuilder::cups().get_printers()).await?;

        Some(
            resp.attributes()
                .groups_of(ipp::model::DelimiterTag::PrinterAttributes)
                .map(|group| {
                    let uri = group.attributes()["printer-uri-supported"]
                        .value()
                        .to_string();
                    let name = group.attributes()["printer-name"].value().to_string();
                    (uri, name)
                })
                .collect(),
        )
    } else {
        None
    };

    Ok(devices)
}

async fn printers(
    State(state): State<Arc<AppState>>,
    method: Method,
    request_type: RequestType,
    form: Result<Form<PrinterForm>, FormRejection>,
) -> Result<Response, AppError> {
    match method {
        Method::GET => {
            let label_sizes = label_size::Entity::find()
                .order_by_asc(label_size::Column::Width)
                .order_by_asc(label_size::Column::Height)
                .all(&state.db)
                .await?;

            Ok(into_response(&AddPrinterTemplate {
                form: PrinterFormTemplate {
                    cups_devices: cups_devices(state.ipp.as_ref()).await?,
                    current_size: None,
                    label_sizes,

                    unique_id: "".to_string(),
                    name: "".to_string(),
                    dpmm: 8,
                    connection: None,
                },
            }))
        }
        Method::POST => {
            let Form(form) = form?;
            let connection = form
                .connection
                .ok_or_else(|| eyre::eyre!("missing connection"))?;

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
                unique_id: Set(form.unique_id),
                id: Set(Uuid::now_v7()),
                name: Set(form.name),
                dpmm: Set(form.dpmm),
                label_size_id,
                connection: Set(serde_json::to_value(connection)?),
            };

            printer.insert(&state.db).await?;

            hx_load(&request_type, false, "/labels")
        }
        _ => Err(eyre::eyre!("unknown method").into()),
    }
}

#[derive(Template)]
#[template(path = "printers/show.html")]
struct PrinterTemplate {
    printer: printer::Model,
    form: PrinterFormTemplate,
}

async fn printer(
    State(state): State<Arc<AppState>>,
    method: Method,
    request_type: RequestType,
    Path(id): Path<UrlId>,
    form: Result<Form<PrinterForm>, FormRejection>,
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
        Method::GET => Ok(into_response(&PrinterTemplate {
            form: PrinterFormTemplate {
                cups_devices: cups_devices(state.ipp.as_ref()).await?,
                current_size,
                label_sizes,

                unique_id: printer.unique_id.clone().unwrap_or_default(),
                connection: serde_json::from_value(printer.connection.clone())?,
                name: printer.name.clone(),
                dpmm: printer.dpmm,
            },
            printer,
        })),
        Method::PUT => {
            let Form(form) = form?;
            let connection = form
                .connection
                .ok_or_else(|| eyre::eyre!("missing connection"))?;

            let mut printer: printer::ActiveModel = printer.into();
            printer.unique_id = Set(form.unique_id);
            printer.name = Set(form.name);
            printer.connection = Set(serde_json::to_value(connection)?);
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
    fn selected_dpmm(&self) -> &i16 {
        match &self.label {
            Some(label) => &label.dpmm,
            None => &8,
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

async fn playground(
    State(state): State<Arc<AppState>>,
    path_id: Option<Path<UrlId>>,
    request_type: RequestType,
    form: Result<Form<PlaygroundForm>, FormRejection>,
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

    let (label_sizes, printers) = try_join!(
        label_size::Entity::find()
            .order_by_asc(label_size::Column::Width)
            .order_by_asc(label_size::Column::Height)
            .all(&state.db),
        printer::Entity::find()
            .order_by_asc(printer::Column::Name)
            .all(&state.db)
    )?;

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

    let variables = match VariableExtractor::extract(&zpl) {
        Ok(variables) => variables,
        Err(_err) if request_type.is_htmx() => return Ok(StatusCode::NO_CONTENT.into_response()),
        Err(err) => {
            return Ok(into_response(&PlaygroundTemplate {
                zpl: &zpl,
                fields: ZplFieldsTemplate::default(),
                rendered: Err(get_template_error(err)),
                preview: None,
                label_sizes,
                printers,
                label,
            }));
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
    let rendered = match render_label(&zpl, &context) {
        Ok(rendered) => Ok(rendered),
        Err(err) => Err(get_template_error(err)),
    };

    let fields = ZplFieldsTemplate { fields };

    let image = if let Ok(zpl) = &rendered {
        render_zpl(&state.image_cache, &state.client, zpl, &label_size, dpmm)
            .await
            .tap_err(|err| tracing::error!("could not get zpl preview: {err}"))
            .ok()
            .as_deref()
            .map(encode_image_html)
    } else {
        None
    };

    if request_type.is_htmx() {
        return Ok(into_response(&PlaygroundUpdateTemplate {
            fields,
            rendered,
            preview: image,
        }));
    }

    Ok(into_response(&PlaygroundTemplate {
        zpl: &zpl,
        fields,
        rendered,
        preview: image,
        label_sizes,
        printers,
        label,
    }))
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

    let (label_id, real_label) = if let Some(id) = form.id {
        (id.into(), true)
    } else {
        (Uuid::now_v7(), false)
    };

    send_print_job(
        &state.db,
        state.ipp.as_ref(),
        printer,
        label::Model {
            id: label_id,
            name: "".to_string(),
            label_size_id: form
                .label_size_id
                .map(Into::into)
                .unwrap_or_else(Uuid::now_v7),
            dpmm: form.dpmm,
            zpl: form.zpl,
        },
        form.fields,
        state.skip,
        real_label,
    )
    .await?;

    Ok(StatusCode::NO_CONTENT)
}

type HistoryValues<'a> = Vec<(
    &'a history::Model,
    String,
    Option<Vec<BTreeMap<u16, String>>>,
    Option<printer::Model>,
    Option<label::Model>,
)>;

#[derive(Template)]
#[template(path = "history/index.html")]
struct HistoryTemplate<'a> {
    history: HistoryValues<'a>,
    next_page: Option<UrlId>,
    prev_page: Option<UrlId>,
}

#[derive(Deserialize)]
struct HistoryQuery {
    before: Option<UrlId>,
    after: Option<UrlId>,
}

#[derive(Debug, Default, FromQueryResult)]
struct NextItems {
    next_page: Option<bool>,
    prev_page: Option<bool>,
}

fn page_id(page: Option<bool>, id: Option<Uuid>) -> Option<UrlId> {
    if page.unwrap_or_default() {
        id.map(Into::into)
    } else {
        None
    }
}

async fn history(
    State(state): State<Arc<AppState>>,
    Query(query): Query<HistoryQuery>,
) -> Result<Response, AppError> {
    const PAGE_COUNT: u64 = 25;

    let mut history = history::Entity::find().cursor_by(history::Column::Id);

    if let Some(before) = query.before {
        history.before(before.0).last(PAGE_COUNT);
    } else if let Some(after) = query.after {
        history.after(after.0).first(PAGE_COUNT);
    } else {
        history.last(PAGE_COUNT);
    }

    let mut entries = history.all(&state.db).await?;
    entries.reverse();

    let first_id = entries.first().map(|entry| entry.id);
    let last_id = entries.last().map(|entry| entry.id);

    let next_items: NextItems =
        NextItems::find_by_statement(Statement::from_sql_and_values(
            DbBackend::Postgres,
            r#"SELECT exists(SELECT 1 one FROM history WHERE id > $1 LIMIT 1) prev_page, exists(SELECT 1 one FROM history WHERE id < $2 LIMIT 1) next_page"#,
            [first_id.into(), last_id.into()],
        ))
        .one(&state.db)
        .await?
        .unwrap_or_default();

    let variables = entries
        .iter()
        .map(|entry| serde_json::to_string_pretty(&entry.variables).unwrap_or_default());

    let verifications = entries.iter().map(|entry| {
        entry
            .verification
            .as_ref()
            .map(|verification| {
                serde_json::from_value::<Vec<BTreeMap<u16, String>>>(verification.clone()).ok()
            })
            .unwrap_or_default()
    });

    let (printers, labels): (Vec<Option<printer::Model>>, Vec<Option<label::Model>>) = try_join!(
        entries.load_one(printer::Entity, &state.db),
        entries.load_one(label::Entity, &state.db)
    )?;

    let history = izip!(&entries, variables, verifications, printers, labels).collect_vec();

    Ok(into_response(&HistoryTemplate {
        history,
        prev_page: page_id(next_items.prev_page, first_id),
        next_page: page_id(next_items.next_page, last_id),
    }))
}

type AlertsValues<'a> = Vec<(&'a alert::Model, Option<printer::Model>)>;

#[derive(Template)]
#[template(path = "alerts/index.html")]
struct AlertsTemplate<'a> {
    alerts: AlertsValues<'a>,
    next_page: Option<UrlId>,
    prev_page: Option<UrlId>,
}

async fn alerts(
    State(state): State<Arc<AppState>>,
    Query(query): Query<HistoryQuery>,
) -> Result<Response, AppError> {
    const PAGE_COUNT: u64 = 25;

    let mut alerts = alert::Entity::find().cursor_by(alert::Column::Id);

    if let Some(before) = query.before {
        alerts.before(before.0).last(PAGE_COUNT);
    } else if let Some(after) = query.after {
        alerts.after(after.0).first(PAGE_COUNT);
    } else {
        alerts.last(PAGE_COUNT);
    }

    let mut entries = alerts.all(&state.db).await?;
    entries.reverse();

    let first_id = entries.first().map(|entry| entry.id);
    let last_id = entries.last().map(|entry| entry.id);

    let next_items: NextItems =
        NextItems::find_by_statement(Statement::from_sql_and_values(
            DbBackend::Postgres,
            r#"SELECT exists(SELECT 1 one FROM alert WHERE id > $1 LIMIT 1) prev_page, exists(SELECT 1 one FROM alert WHERE id < $2 LIMIT 1) next_page"#,
            [first_id.into(), last_id.into()],
        ))
        .one(&state.db)
        .await?
        .unwrap_or_default();

    let printers: Vec<Option<printer::Model>> =
        entries.load_one(printer::Entity, &state.db).await?;

    let alerts = izip!(&entries, printers).collect_vec();

    Ok(into_response(&AlertsTemplate {
        alerts,
        prev_page: page_id(next_items.prev_page, first_id),
        next_page: page_id(next_items.next_page, last_id),
    }))
}

struct ImageForm {
    existing_image: Option<String>,
    preview: Option<String>,
    zpl: Option<String>,
    dithering_type: zpl::DitheringType,
    encoding_method: zpl::BinaryEncodingMethod,
    width: String,
    height: String,
    printers: Vec<printer::Model>,
    file_name: String,
    storage_device: String,
    printer_id: Uuid,
}

impl Default for ImageForm {
    fn default() -> Self {
        Self {
            existing_image: None,
            preview: None,
            zpl: None,
            dithering_type: zpl::DitheringType::Ordered,
            encoding_method: zpl::BinaryEncodingMethod::Hex,
            width: "".to_string(),
            height: "".to_string(),
            printers: Default::default(),
            file_name: "".to_string(),
            storage_device: "R:".to_string(),
            printer_id: Uuid::nil(),
        }
    }
}

#[derive(Template)]
#[template(path = "images/index.html")]
struct ImageTemplate {
    form: ImageForm,
}

#[derive(Template)]
#[template(path = "images/index.html", block = "image_form")]
struct ImageFormTemplate {
    form: ImageForm,
}

async fn parse_field<T: serde::de::DeserializeOwned>(field: Field<'_>) -> eyre::Result<T> {
    parse_field_opt(field)
        .await
        .transpose()
        .ok_or_else(|| eyre::eyre!("field was missing value"))?
}

async fn parse_field_opt<T: serde::de::DeserializeOwned>(
    field: Field<'_>,
) -> eyre::Result<Option<T>> {
    let data = String::from_utf8(field.bytes().await?.to_vec())?;
    if data.is_empty() {
        return Ok(None);
    }
    Ok(Some(serde_plain::from_str(&data)?))
}

async fn images(
    State(state): State<Arc<AppState>>,
    method: Method,
    form: Result<Multipart, MultipartRejection>,
) -> Result<Response, AppError> {
    let printers = printer::Entity::find()
        .order_by_asc(printer::Column::Name)
        .all(&state.db)
        .await?;

    match (method, form) {
        (Method::GET, _) => Ok(into_response(&ImageTemplate {
            form: ImageForm {
                printers,
                ..Default::default()
            },
        })),
        (Method::POST, Ok(mut form)) => {
            let mut action: Option<String> = None;
            let mut dithering_type = zpl::DitheringType::Ordered;
            let mut encoding_method = zpl::BinaryEncodingMethod::Hex;
            let mut src_width: Option<u32> = None;
            let mut src_height: Option<u32> = None;
            let mut file_name: Option<String> = None;
            let mut storage_device: Option<String> = None;
            let mut printer_id: Option<UrlId> = None;

            let mut image_data: Option<Vec<u8>> = None;

            while let Some(field) = form.next_field().await? {
                match field.name() {
                    Some("image") => image_data = Some(field.bytes().await?.to_vec()),
                    Some("existing_image") => {
                        image_data =
                            Some(base64::prelude::BASE64_URL_SAFE.decode(&field.bytes().await?)?)
                    }
                    Some("dithering_type") => dithering_type = parse_field(field).await?,
                    Some("encoding_method") => encoding_method = parse_field(field).await?,
                    Some("width") => src_width = parse_field_opt(field).await?,
                    Some("height") => src_height = parse_field_opt(field).await?,
                    Some("file_name") => file_name = parse_field_opt(field).await?,
                    Some("storage_device") => storage_device = parse_field_opt(field).await?,
                    Some("printer_id") => printer_id = parse_field_opt(field).await?,
                    Some("action") => action = parse_field_opt(field).await?,
                    _ => continue,
                }
            }

            tracing::debug!(
                ?action,
                ?dithering_type,
                ?encoding_method,
                ?src_width,
                ?src_height,
                ?file_name,
                ?storage_device,
                ?printer_id,
                "got fields"
            );

            let Some(image_data) = image_data else {
                return Err(eyre::eyre!("missing image").into());
            };

            let mut im = image::load_from_memory(&image_data)?;

            if let (Some(width), Some(height)) = (src_width, src_height) {
                im = im.resize(width, height, Lanczos3);
            }

            let (im, height, width_padded) = zpl::process_image(&im, Some(dithering_type));
            let (field_data, line_size, total_size) =
                zpl::encode_image(height, width_padded, encoding_method, &im);

            if matches!(action.as_deref(), Some("Store")) {
                let (Some(file_name), Some(storage_device), Some(printer_id)) =
                    (file_name.as_deref(), storage_device.as_deref(), printer_id)
                else {
                    return Err(eyre::eyre!("missing parameters for store action").into());
                };

                let printer = printer::Entity::find_by_id(printer_id.0)
                    .one(&state.db)
                    .await?
                    .ok_or_else(|| eyre::eyre!("missing printer"))?;

                let label = label::Model {
                    id: Uuid::nil(),
                    name: "Store Graphic".to_string(),
                    label_size_id: printer.label_size_id.unwrap_or_default(),
                    dpmm: 0,
                    zpl: format!(
                        "~DG{storage_device}{file_name}.GRF,{total_size},{line_size},{field_data}"
                    ),
                };

                crate::send_print_job(
                    &state.db,
                    state.ipp.as_ref(),
                    printer,
                    label,
                    Default::default(),
                    state.skip,
                    false,
                )
                .await?;
            }

            let mut buf = Vec::new();
            let png = image::codecs::png::PngEncoder::new(&mut buf);
            png.write_image(
                im.as_bytes(),
                width_padded,
                height,
                image::ColorType::L8.into(),
            )?;

            Ok(into_response(&ImageFormTemplate {
                form: ImageForm {
                    existing_image: Some(base64::prelude::BASE64_URL_SAFE.encode(&image_data)),
                    preview: Some(encode_image_html(&buf)),
                    zpl: Some(format!(
                        "^GFA,{total_size},{total_size},{line_size},{field_data}"
                    )),
                    dithering_type,
                    encoding_method,
                    width: src_width.map(|val| val.to_string()).unwrap_or_default(),
                    height: src_height.map(|val| val.to_string()).unwrap_or_default(),
                    printers,
                    file_name: file_name.unwrap_or_default(),
                    storage_device: storage_device.unwrap_or_else(|| "R:".to_string()),
                    printer_id: printer_id.map(|id| id.0).unwrap_or_else(Uuid::nil),
                },
            }))
        }
        _ => Err(eyre::eyre!("unknown method").into()),
    }
}
