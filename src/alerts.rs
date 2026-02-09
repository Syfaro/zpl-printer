use std::time::Duration;
use std::{net::SocketAddr, sync::Arc};

use async_trait::async_trait;
use lazy_static::lazy_static;
use regex::Regex;
use sea_orm::{ActiveValue::Set, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter};
use tokio::task::JoinHandle;
use tokio::time::timeout;
use tokio::{
    io::AsyncReadExt,
    net::{TcpListener, TcpStream},
    task::AbortHandle,
};
use tokio_util::sync::CancellationToken;
use tracing::{debug, warn};
use uuid::Uuid;

use crate::entities::{alert, printer};

lazy_static! {
    static ref MESSAGE_REGEX: Regex = Regex::new(
        r"^(?P<alert_type>[A-Z ]+): (?P<alert_message>[A-Z ]+) \[(?P<timestamp>[0-9 :-]+)\] \[(?P<unique_id>\w+)\]",
    ).unwrap();
}

pub struct AlertHandler {
    db: DatabaseConnection,
    notify_for_completion: bool,

    targets: Vec<Box<dyn AlertTarget>>,
}

impl AlertHandler {
    pub fn new(db: DatabaseConnection, notify_for_completion: bool) -> Self {
        Self {
            db,
            notify_for_completion,
            targets: Vec::new(),
        }
    }

    pub fn add_target(&mut self, target: Box<dyn AlertTarget>) {
        self.targets.push(target);
    }

    pub async fn process(&self, mut alert: alert::ActiveModel) -> eyre::Result<()> {
        let Set(unique_id) = alert.unique_id.clone() else {
            eyre::bail!("unique id must always be set");
        };

        let printer = printer::Entity::find()
            .filter(printer::Column::UniqueId.eq(&unique_id))
            .one(&self.db)
            .await?;

        if alert.id.is_not_set() {
            alert.id = Set(Uuid::now_v7());
        }
        alert.printer_id = Set(printer.as_ref().map(|printer| printer.id));

        let is_complete_alert = alert.alert_type == Set("ALERT".to_string())
            && alert.alert_message == Set("PQ JOB COMPLETED".to_string());

        let alert = alert::Entity::insert(alert)
            .exec_with_returning(&self.db)
            .await?;
        debug!(is_complete_alert, "recorded alert");

        if self.notify_for_completion || !is_complete_alert {
            for target in self.targets.iter() {
                target.send(&alert, printer.as_ref()).await?;
            }
        }

        Ok(())
    }
}

#[async_trait]
pub trait AlertTarget: Send + Sync {
    async fn send(
        &self,
        alert: &alert::Model,
        printer: Option<&printer::Model>,
    ) -> eyre::Result<()>;
}

pub struct TelegramAlertTarget {
    client: reqwest::Client,
    api_token: String,
    chat_id: i64,
}

impl TelegramAlertTarget {
    pub fn new(api_token: String, chat_id: i64) -> Self {
        Self {
            api_token,
            chat_id,
            client: Default::default(),
        }
    }
}

#[async_trait]
impl AlertTarget for TelegramAlertTarget {
    async fn send(
        &self,
        alert: &alert::Model,
        printer: Option<&printer::Model>,
    ) -> eyre::Result<()> {
        let name = printer
            .as_ref()
            .map(|printer| printer.name.as_str())
            .unwrap_or(&alert.unique_id);

        let text = format!(
            "{name} Printer Alert!\n\n{}: {}",
            alert.alert_type, alert.alert_message
        );

        self.client
            .post(format!(
                "https://api.telegram.org/bot{}/sendMessage",
                self.api_token
            ))
            .json(&serde_json::json!({
                "chat_id": self.chat_id,
                "text": text,
            }))
            .send()
            .await?
            .error_for_status()?;

        Ok(())
    }
}

pub async fn alert_listener(
    handler: Arc<AlertHandler>,
    addr: SocketAddr,
    token: CancellationToken,
) -> eyre::Result<AbortHandle> {
    let listener = TcpListener::bind(addr).await?;

    let task: JoinHandle<eyre::Result<()>> = tokio::spawn(async move {
        loop {
            tokio::select! {
                _ = token.cancelled() => {
                    break;
                }

                res = listener.accept() => {
                    let (socket, _) = res?;

                    match read_incoming_alert(socket).await {
                        Ok(alert) => {
                            handler.process(alert).await?;
                        }

                        Err(err) => {
                            warn!("could not read incoming alert: {err}");
                        }
                    }
                }
            }
        }

        Ok(())
    });

    Ok(task.abort_handle())
}

async fn read_incoming_alert(mut socket: TcpStream) -> eyre::Result<alert::ActiveModel> {
    let mut buf = Vec::with_capacity(1024);
    timeout(Duration::from_secs(3), socket.read_to_end(&mut buf)).await??;
    let s = String::from_utf8(buf)?;

    let captures = MESSAGE_REGEX
        .captures(&s)
        .ok_or_else(|| eyre::eyre!("alert data could not be parsed: {s}"))?;

    let printer_timestamp =
        chrono::NaiveDateTime::parse_from_str(&captures["timestamp"], "%Y-%m-%d %H:%M:%S")?;

    let alert = alert::ActiveModel {
        alert_type: Set(captures["alert_type"].to_string()),
        alert_message: Set(captures["alert_message"].to_string()),
        printer_timestamp: Set(printer_timestamp),
        unique_id: Set(captures["unique_id"].to_string()),
        ..Default::default()
    };

    Ok(alert)
}
