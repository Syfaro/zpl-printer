use std::{
    collections::{BTreeMap, HashMap},
    error::Error,
    net::SocketAddr,
    sync::Arc,
};

use askama::Template;
use askama_axum::IntoResponse;
use axum::{
    extract::{Path, Query, State},
    http::{Method, StatusCode},
    response::{Redirect, Response},
    routing::{delete, get, post, put},
    Form, Router,
};
use base64::Engine;
use itertools::{izip, Itertools};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use sea_orm::{
    ActiveModelTrait, ActiveValue::NotSet, CursorTrait, DbBackend, EntityTrait, FromQueryResult,
    LoaderTrait, QueryOrder, Set, Statement,
};
use serde::Deserialize;
use serde_with::{serde_as, NoneAsEmptyString};
use tap::TapFallible;
use tokio::try_join;
use uuid::Uuid;

use crate::{
    entities::*,
    render_zpl, send_print_job,
    template::{render_label, VariableExtractor},
    web::{hx_load, AppState, AsUrl, RequestType, UrlId},
    AppError,
};

pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/", get(|| async { Redirect::to("/labels") }))
        .route("/labels", get(labels).post(label))
        .route("/labels/:id", put(label).delete(label))
        .route("/label_sizes", post(label_sizes))
        .route("/label_sizes/:id", delete(label_size))
        .route("/printers", post(printers))
        .route("/printers/:id", get(printer).put(printer))
        .route("/playground", get(playground).post(playground))
        .route("/playground/:id", get(playground))
        .route("/playground/print/:id", post(playground_print))
        .route("/history", get(history))
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

#[derive(Template)]
#[template(path = "history/index.html")]
struct HistoryTemplate<'a> {
    history: Vec<(
        &'a history::Model,
        String,
        Option<printer::Model>,
        Option<label::Model>,
    )>,

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

    let (printers, labels): (Vec<Option<printer::Model>>, Vec<Option<label::Model>>) = try_join!(
        entries.load_one(printer::Entity, &state.db),
        entries.load_one(label::Entity, &state.db)
    )?;

    let history = izip!(&entries, variables, printers, labels).collect_vec();

    Ok(HistoryTemplate {
        history,
        prev_page: page_id(next_items.prev_page, first_id),
        next_page: page_id(next_items.next_page, last_id),
    }
    .into_response())
}
