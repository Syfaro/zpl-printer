use std::{
    collections::HashMap,
    sync::{Arc, atomic::AtomicUsize},
    time::Duration,
};

use async_trait::async_trait;
use bytes::Bytes;
use futures_util::TryStreamExt;
use tokio::{
    io::AsyncRead,
    sync::{Mutex, broadcast, mpsc, oneshot},
    task::AbortHandle,
    time::interval,
};
use tokio_stream::wrappers::BroadcastStream;
use tokio_util::{io::StreamReader, sync::CancellationToken, task::TaskTracker};
use tracing::trace;
use uuid::Uuid;

use crate::weblink::ZebraChannel;

pub type PrinterChannel = (Uuid, ZebraChannel);
pub type ActionWithSender = (TransportAction, oneshot::Sender<eyre::Result<()>>);

#[derive(Debug)]
pub enum TransportAction {
    Disconnect,
    Write(Bytes),
}

#[async_trait]
pub trait Transport: Send + Sync + 'static {
    async fn prepare(
        &self,
        printer: PrinterChannel,
        token: CancellationToken,
    ) -> eyre::Result<(mpsc::Receiver<ActionWithSender>, broadcast::Sender<Bytes>)>;

    async fn perform_action(
        &self,
        printer: &PrinterChannel,
        action: TransportAction,
    ) -> eyre::Result<()>;

    async fn get_handle(&self, printer: &PrinterChannel) -> eyre::Result<Box<dyn PrinterHandle>>;
}

#[async_trait]
pub trait PrinterHandle: Send + Sync + 'static {
    async fn perform_action(&self, action: TransportAction) -> eyre::Result<()>;
    async fn reader(&self) -> Box<dyn AsyncRead + Send + Sync + Unpin>;
}

pub struct InMemoryTransport {
    printers: Arc<Mutex<HashMap<PrinterChannel, InMemoryPrinter>>>,
    metrics_handle: AbortHandle,
    cleanup_tasks: TaskTracker,
}

struct InMemoryPrinter {
    id: usize,
    token: CancellationToken,
    action_tx: mpsc::Sender<ActionWithSender>,
    data_tx: broadcast::Sender<Bytes>,
}

pub struct InMemoryPrinterHandle {
    action_tx: mpsc::Sender<ActionWithSender>,
    data_tx: broadcast::Sender<Bytes>,
}

impl InMemoryTransport {
    const CHANNEL_TYPES: [ZebraChannel; 3] = [
        ZebraChannel::Main,
        ZebraChannel::ConfigV1,
        ZebraChannel::RawV1,
    ];

    pub fn new() -> Self {
        let printers = Arc::new(Mutex::new(HashMap::new()));
        let metrics_handle = tokio::spawn(Self::update_metrics(printers.clone())).abort_handle();

        Self {
            printers,
            metrics_handle,
            cleanup_tasks: Default::default(),
        }
    }

    async fn update_metrics(printers: Arc<Mutex<HashMap<PrinterChannel, InMemoryPrinter>>>) {
        let mut interval = interval(Duration::from_secs(5));

        loop {
            interval.tick().await;

            let mut counts: HashMap<ZebraChannel, u64> =
                HashMap::with_capacity(Self::CHANNEL_TYPES.len());

            for ((_, channel), _) in printers.lock().await.iter() {
                *counts.entry(*channel).or_default() += 1;
            }

            for channel_type in Self::CHANNEL_TYPES {
                axum_prometheus::metrics::gauge!(
                    "zpl_printer_weblink_channels", "channel" => channel_type.to_string()
                )
                .set(counts.get(&channel_type).copied().unwrap_or_default() as f64);
            }
        }
    }

    async fn cleanup_task(
        printers: Arc<Mutex<HashMap<PrinterChannel, InMemoryPrinter>>>,
        printer: PrinterChannel,
        token: CancellationToken,
        id: usize,
    ) {
        token.cancelled().await;

        let mut printers = printers.lock().await;
        let current_id = printers.get(&printer).map(|printer| printer.id);
        if current_id == Some(id)
            && let Some(printer) = printers.remove(&printer)
        {
            trace!("removing printer from transport");
            printer.token.cancel();
        }
    }

    fn get_next_id() -> usize {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }
}

#[async_trait]
impl PrinterHandle for InMemoryPrinterHandle {
    async fn perform_action(&self, action: TransportAction) -> eyre::Result<()> {
        let (tx, rx) = oneshot::channel();
        self.action_tx.send((action, tx)).await?;
        rx.await??;

        Ok(())
    }

    async fn reader(&self) -> Box<dyn AsyncRead + Send + Sync + Unpin> {
        Box::new(StreamReader::new(
            BroadcastStream::new(self.data_tx.subscribe()).map_err(std::io::Error::other),
        ))
    }
}

#[async_trait]
impl Transport for InMemoryTransport {
    async fn prepare(
        &self,
        printer: PrinterChannel,
        token: CancellationToken,
    ) -> eyre::Result<(mpsc::Receiver<ActionWithSender>, broadcast::Sender<Bytes>)> {
        let (action_tx, action_rx) = mpsc::channel(1);
        let (data_tx, _) = broadcast::channel(1);

        let id = Self::get_next_id();
        let new_printer = InMemoryPrinter {
            id,
            token: token.clone(),
            action_tx,
            data_tx: data_tx.clone(),
        };

        if let Some(previous_printer) = self.printers.lock().await.insert(printer, new_printer) {
            trace!("removing stale printer connection");
            previous_printer.token.cancel();
        }

        self.cleanup_tasks.spawn(Self::cleanup_task(
            self.printers.clone(),
            printer,
            token,
            id,
        ));

        Ok((action_rx, data_tx))
    }

    async fn perform_action(
        &self,
        printer: &PrinterChannel,
        action: TransportAction,
    ) -> eyre::Result<()> {
        let Some(action_tx) = self
            .printers
            .lock()
            .await
            .get(printer)
            .map(|printer| printer.action_tx.clone())
        else {
            eyre::bail!("printer not connected");
        };

        let (tx, rx) = oneshot::channel();
        action_tx.send((action, tx)).await?;
        rx.await??;

        Ok(())
    }

    async fn get_handle(&self, printer: &PrinterChannel) -> eyre::Result<Box<dyn PrinterHandle>> {
        let Some((action_tx, data_tx)) = self
            .printers
            .lock()
            .await
            .get(printer)
            .map(|printer| (printer.action_tx.clone(), printer.data_tx.clone()))
        else {
            eyre::bail!("printer not connected");
        };

        Ok(Box::new(InMemoryPrinterHandle { action_tx, data_tx }))
    }
}

impl Drop for InMemoryTransport {
    fn drop(&mut self) {
        self.metrics_handle.abort();
        self.cleanup_tasks.close();
    }
}
