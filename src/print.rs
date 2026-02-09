use std::{collections::HashMap, io::Cursor, net::SocketAddr, sync::Arc, time::Duration};

use async_trait::async_trait;
use axum_prometheus::metrics::counter;
use bytes::Bytes;
use ipp::prelude::{AsyncIppClient, IppOperationBuilder, IppPayload};
use memchr::memmem::Finder;
use sea_orm::{
    ActiveModelTrait,
    ActiveValue::{NotSet, Set},
    ColumnTrait, DatabaseConnection, EntityTrait, PaginatorTrait, QueryFilter, QueryOrder,
};
use tap::TapFallible;
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, AsyncRead, AsyncReadExt, AsyncWriteExt, BufReader},
    net::{TcpStream, tcp::OwnedWriteHalf},
    time::timeout,
};
use tracing::{debug, error, info, instrument, trace, warn};
use uuid::Uuid;

use crate::{
    entities::{history, printer, web_link_certificate},
    template,
    transport::{PrinterHandle, TransportAction},
    web::{PrinterConnection, WebLinkState},
    weblink::ZebraChannel,
    zpl::{ZplHostVerification, ZplLabel, ZplParser},
};

type LabelValidations = Vec<HashMap<u16, String>>;

pub struct Print {
    db: DatabaseConnection,
    weblink: Option<Arc<WebLinkState>>,
    ipp: Option<Arc<AsyncIppClient>>,
    skip: bool,
}

impl Print {
    pub fn new(
        db: DatabaseConnection,
        weblink: Option<Arc<WebLinkState>>,
        ipp: Option<Arc<AsyncIppClient>>,
        skip: bool,
    ) -> Self {
        Self {
            db,
            weblink,
            ipp,
            skip,
        }
    }

    #[instrument(skip_all)]
    pub async fn send_label(
        &self,
        printer: printer::Model,
        zpl: &str,
        variables: HashMap<String, String>,
        label_size_id: Option<Uuid>,
        label_id: Option<Uuid>,
    ) -> eyre::Result<(history::Model, Option<LabelValidations>)> {
        if printer.label_size_id.is_some()
            && label_size_id.is_some()
            && printer.label_size_id != label_size_id
        {
            return Err(eyre::eyre!("label size does not match printer"));
        }

        let variables: HashMap<String, String> = variables
            .into_iter()
            .filter(|(_key, value)| !value.is_empty())
            .collect();

        let context = tera::Context::from_serialize(variables.clone())?;
        let data = template::render_label(zpl, &context)?;

        let commands = ZplParser::parse(&data)
            .tap_err(|err| error!("could not parse zpl: {err}"))
            .unwrap_or_default();
        let labels = ZplParser::split_labels(&commands);

        let print_quantity = labels.iter().map(|label| label.quantity() as i32).sum();

        trace!(print_quantity, "finished parsing zpl");

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

        let history = history::Entity::insert(history)
            .exec_with_returning(&self.db)
            .await?;

        if self.skip {
            warn!("skip enabled, label not printed");
            return Ok((history, None));
        }

        let (mut connected_printer, rdr) = self.get_connected_printer(&printer).await?;
        let mut rdr = BufReader::new(rdr);

        if !self
            .check_status(connected_printer.as_mut(), &mut rdr)
            .await?
        {
            eyre::bail!("printer reported it was not ready");
        }

        connected_printer.write(data.as_bytes()).await?;

        let output = if connected_printer.can_read() {
            self.get_host_verifications(history, &labels, &mut rdr)
                .await?
        } else {
            (history, None)
        };

        connected_printer.close().await?;

        info!("finished print job");

        counter!("zpl_printer_print_label_total").increment(1);
        counter!("zpl_printer_print_label_printers", "printer" => printer.id.to_string())
            .increment(1);
        if let Some(label_id) = label_id {
            counter!("zpl_printer_print_label_formats", "label" => label_id.to_string())
                .increment(1);
        }

        Ok(output)
    }

    pub async fn send_raw(&self, printer: printer::Model, data: Bytes) -> eyre::Result<()> {
        if self.skip {
            warn!("skip enabled, label not printed");
            return Ok(());
        }

        let (mut connected_printer, mut rdr) = self.get_connected_printer(&printer).await?;

        if let Ok(s) = simdutf8::basic::from_utf8(&data) {
            trace!("input was utf8, checking if response is expected, {s}");
        }

        if !self
            .check_status(connected_printer.as_mut(), &mut rdr)
            .await?
        {
            eyre::bail!("printer reported it was not ready");
        }

        connected_printer.write(&data).await?;
        connected_printer.close().await?;

        info!("finished sending raw");

        counter!("zpl_printer_print_raw_total").increment(1);
        counter!("zpl_printer_print_raw_printers", "printer" => printer.id.to_string())
            .increment(1);

        Ok(())
    }

    async fn get_connected_printer(
        &self,
        printer: &printer::Model,
    ) -> eyre::Result<(Box<dyn ConnectedPrinter>, Box<dyn AsyncRead + Send + Unpin>)> {
        Ok(
            if let Some(handle) = self.get_weblink_handle(printer).await? {
                WeblinkConnectedPrinter::open(handle).await
            } else {
                let connection = printer
                    .connection
                    .clone()
                    .map(serde_json::from_value)
                    .transpose()?
                    .ok_or_else(|| eyre::eyre!("printer had no configured connection"))?;

                match connection {
                    PrinterConnection::Network { address } => {
                        TcpConnectedPrinter::open(address).await?
                    }
                    PrinterConnection::Cups { uri } => {
                        let Some(ipp) = self.ipp.as_ref() else {
                            eyre::bail!("cups printer but not configured to use cups");
                        };

                        CupsConnectedPrinter::open(ipp.clone(), uri)
                    }
                }
            },
        )
    }

    async fn get_weblink_handle(
        &self,
        printer: &printer::Model,
    ) -> eyre::Result<Option<Box<dyn PrinterHandle>>> {
        if let Some(weblink_state) = self.weblink.as_ref() {
            let recently_used = web_link_certificate::Entity::find()
                .filter(
                    web_link_certificate::Column::PrinterId.eq(printer.id).and(
                        web_link_certificate::Column::LastPingAt
                            .gt(chrono::Utc::now() - chrono::Duration::minutes(2)),
                    ),
                )
                .order_by_desc(web_link_certificate::Column::LastPingAt)
                .count(&self.db)
                .await?;

            if recently_used == 0 {
                return Ok(None);
            }

            if let Ok(handle) = weblink_state
                .transport
                .get_handle(&(printer.id, ZebraChannel::RawV1))
                .await
                .tap_err(|err| warn!("had recent ping but could not get handle: {err}"))
            {
                return Ok(Some(handle));
            }
        }

        Ok(None)
    }

    async fn get_host_verifications<R>(
        &self,
        history: history::Model,
        labels: &[ZplLabel<'_>],
        mut rdr: R,
    ) -> eyre::Result<(history::Model, Option<LabelValidations>)>
    where
        R: AsyncRead + AsyncBufRead + Unpin,
    {
        let verifications = match check_host_verifications(&mut rdr, labels).await {
            Ok(verifications) => verifications,
            Err(err) => {
                warn!("could not get verifications: {err}");
                return Ok((history, None));
            }
        };

        let mut active_model: history::ActiveModel = history.clone().into();
        active_model.verification = Set(serde_json::to_value(&verifications)?.into());
        let history = active_model.update(&self.db).await?;

        Ok((history, Some(verifications)))
    }

    async fn check_status(
        &self,
        connected_printer: &mut dyn ConnectedPrinter,
        rdr: &mut (dyn AsyncRead + Send + Unpin),
    ) -> eyre::Result<bool> {
        if !connected_printer.can_read() {
            return Ok(true);
        }

        connected_printer.write(b"~HS\r\n").await?;

        let mut buf = vec![0; 82];
        rdr.read_exact(&mut buf).await?;

        let s = String::from_utf8_lossy(&buf);
        info!("got status result: {s}");

        Ok(true)
    }
}

#[async_trait]
trait ConnectedPrinter: Send + Sync {
    fn can_read(&self) -> bool;

    async fn write(&mut self, data: &[u8]) -> eyre::Result<()>;
    async fn close(&mut self) -> eyre::Result<()>;
}

struct TcpConnectedPrinter {
    wtr: OwnedWriteHalf,
}

impl TcpConnectedPrinter {
    async fn open(
        address: SocketAddr,
    ) -> eyre::Result<(Box<dyn ConnectedPrinter>, Box<dyn AsyncRead + Send + Unpin>)> {
        let stream = TcpStream::connect(address).await?;
        let (rdr, wtr) = stream.into_split();
        Ok((Box::new(Self { wtr }), Box::new(rdr)))
    }
}

#[async_trait]
impl ConnectedPrinter for TcpConnectedPrinter {
    fn can_read(&self) -> bool {
        true
    }

    async fn write(&mut self, data: &[u8]) -> eyre::Result<()> {
        self.wtr.write_all(data).await?;
        Ok(())
    }

    async fn close(&mut self) -> eyre::Result<()> {
        self.wtr.shutdown().await?;
        Ok(())
    }
}

struct WeblinkConnectedPrinter {
    handle: Box<dyn PrinterHandle>,
}

impl WeblinkConnectedPrinter {
    async fn open(
        handle: Box<dyn PrinterHandle>,
    ) -> (Box<dyn ConnectedPrinter>, Box<dyn AsyncRead + Send + Unpin>) {
        let reader = handle.reader().await;
        (Box::new(Self { handle }), reader)
    }
}

#[async_trait]
impl ConnectedPrinter for WeblinkConnectedPrinter {
    fn can_read(&self) -> bool {
        true
    }

    async fn write(&mut self, data: &[u8]) -> eyre::Result<()> {
        self.handle
            .perform_action(TransportAction::Write(Bytes::copy_from_slice(data)))
            .await?;
        Ok(())
    }

    async fn close(&mut self) -> eyre::Result<()> {
        Ok(())
    }
}

struct CupsConnectedPrinter {
    client: Arc<AsyncIppClient>,
    uri: String,
}

impl CupsConnectedPrinter {
    fn open(
        client: Arc<AsyncIppClient>,
        uri: String,
    ) -> (Box<dyn ConnectedPrinter>, Box<dyn AsyncRead + Send + Unpin>) {
        (Box::new(Self { client, uri }), Box::new(tokio::io::empty()))
    }
}

#[async_trait]
impl ConnectedPrinter for CupsConnectedPrinter {
    fn can_read(&self) -> bool {
        false
    }

    async fn write(&mut self, data: &[u8]) -> eyre::Result<()> {
        let payload = IppPayload::new(Cursor::new(Bytes::copy_from_slice(data)));
        let op = IppOperationBuilder::print_job(self.uri.parse()?, payload).build();

        let resp = self.client.send(op).await?;
        info!(status_code = %resp.header().status_code(), "got print status code");

        Ok(())
    }

    async fn close(&mut self) -> eyre::Result<()> {
        Ok(())
    }
}

async fn check_host_verifications<R>(
    mut rdr: R,
    labels: &[ZplLabel<'_>],
) -> eyre::Result<LabelValidations>
where
    R: AsyncBufRead + Unpin,
{
    const DUR: Duration = Duration::from_secs(3);

    // All of the extracted label verifications, by index of the label and then
    // the field number.
    let mut label_verifications: HashMap<usize, HashMap<u16, String>> = HashMap::new();

    let labels_len = labels.len();
    let labels_with_verifications: Vec<_> = labels
        .iter()
        .map(|label| (label, ZplHostVerification::from_commands(label.commands)))
        .collect();

    // TODO: Axum's routing gets unhappy if we don't collect this first, figure
    // out why.
    let host_verifications: Vec<_> = labels_with_verifications
        .iter()
        .enumerate()
        .flat_map(|(label_index, (label, verifications))| {
            label
                .iter_host_verifications(verifications)
                .map(move |(page, verification)| (label_index * labels_len + page, verification))
        })
        .collect();

    if host_verifications.is_empty() {
        return Ok(Vec::new());
    }

    let mut host_verifications = host_verifications.into_iter().peekable();

    while let Some((index, verification)) = host_verifications.next() {
        trace!(?verification, index, "extracting next verification");

        if let Some(prefix) = verification.prefix {
            let mut actual_prefix = vec![0; prefix.len()];
            timeout(DUR, rdr.read_exact(&mut actual_prefix)).await??;

            if actual_prefix != prefix.as_bytes() {
                eyre::bail!(
                    "host verification had wrong prefix, expected {prefix} but got {}",
                    String::from_utf8_lossy(&actual_prefix)
                );
            }
        }

        let data = if let Some(suffix) = verification.suffix {
            let finder = Finder::new(suffix.as_bytes());
            let limit = (verification.length + suffix.len()) as u64;
            let mut take_rdr = (&mut rdr).take(limit);
            trace!(limit, suffix, "preparing to find suffix");

            let mut data = Vec::with_capacity(verification.length);
            loop {
                let buf = timeout(DUR, take_rdr.fill_buf()).await??;
                if buf.is_empty() {
                    eyre::bail!("got eof while waiting for suffix");
                }
                trace!("read more into buffer, {}", String::from_utf8_lossy(buf));

                if let Some(idx) = finder.find(buf) {
                    // Only extend until the suffix, but still consume it.
                    data.extend_from_slice(&buf[..idx]);
                    take_rdr.consume(idx + suffix.len());
                    break;
                } else {
                    let len = buf.len();
                    data.extend_from_slice(buf);
                    take_rdr.consume(len);
                }
            }
            data
        } else if let Some(next_verification_prefix) = host_verifications
            .peek()
            .and_then(|(_, verification)| verification.prefix)
        {
            let finder = Finder::new(next_verification_prefix.as_bytes());
            let limit = (verification.length + next_verification_prefix.len()) as u64;
            let mut take_rdr = (&mut rdr).take(limit);
            trace!(limit, next_verification_prefix, "looking for next prefix");

            let mut data = Vec::with_capacity(verification.length);
            loop {
                let buf = match timeout(DUR, take_rdr.fill_buf()).await {
                    Ok(Ok(buf)) => buf,
                    Ok(Err(err)) => return Err(err.into()),
                    Err(err) => {
                        if host_verifications.peek().is_none() && !data.is_empty() {
                            debug!("reached end without next");
                            break;
                        } else {
                            return Err(err.into());
                        }
                    }
                };
                if buf.is_empty() {
                    if host_verifications.peek().is_none() && !data.is_empty() {
                        break;
                    } else {
                        eyre::bail!("got eof while waiting for suffix");
                    }
                }
                trace!("read more into buffer, {}", String::from_utf8_lossy(buf));

                if let Some(idx) = finder.find(buf) {
                    // Only extend until the prefix, but make sure we don't
                    // consume any of it.
                    trace!(
                        "found next prefix at {idx}, consuming: {}",
                        String::from_utf8_lossy(&buf[..idx])
                    );
                    data.extend_from_slice(&buf[..idx]);
                    take_rdr.consume(idx);
                    break;
                } else if buf.len() >= verification.length {
                    trace!("reached maximum length for value without finding prefix");
                    let len = buf.len();
                    data.extend_from_slice(buf);
                    take_rdr.consume(len);
                    break;
                } else {
                    let len = buf.len();
                    data.extend_from_slice(buf);
                    take_rdr.consume(len);
                }
            }
            data
        } else {
            let mut content = Vec::with_capacity(verification.length);
            let mut buf = vec![0; verification.length];
            while content.len() < verification.length {
                let remaining = verification.length - content.len();
                match timeout(DUR, rdr.read(&mut buf[..remaining])).await {
                    Ok(Ok(n)) => {
                        if n == 0 {
                            if content.is_empty() {
                                eyre::bail!("reached eof");
                            } else {
                                break;
                            }
                        }
                        content.extend_from_slice(&buf[..n]);
                    }
                    Ok(Err(err)) => return Err(err.into()),
                    Err(err) => {
                        if !content.is_empty() && host_verifications.peek().is_none() {
                            break;
                        } else {
                            return Err(err.into());
                        }
                    }
                }
                if content.len() == verification.length {
                    break;
                }
            }

            content
        };

        label_verifications.entry(index).or_default().insert(
            verification.field_no,
            String::from_utf8_lossy(&data).to_string(),
        );
    }

    let verifications = (0..label_verifications.keys().max().copied().unwrap())
        .map(|index| label_verifications.remove(&(index + 1)).unwrap_or_default())
        .collect();

    debug!("got all verifications: {verifications:#?}");

    Ok(verifications)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use bytes::Bytes;
    use futures_util::StreamExt;
    use tokio_util::io::StreamReader;

    use crate::{print::check_host_verifications, zpl::ZplParser};

    #[tokio::test]
    async fn test_correct_host_verifications() {
        let _ = tracing_subscriber::fmt::try_init();

        let commands = ZplParser::parse("^XA^HV1,4,NAME[,],L^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);
        let input = Cursor::new(b"NAME[test]");

        let verifications = check_host_verifications(input, &labels).await.unwrap();
        assert_eq!(
            verifications,
            vec![[(1, "test".to_string())].into_iter().collect()]
        );

        let commands = ZplParser::parse("^XA^HV1,10,,,L^PQ2^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);
        let input = Cursor::new(b"HELLOWORLDHELLOTHERE");

        let verifications = check_host_verifications(input, &labels).await.unwrap();
        assert_eq!(
            verifications,
            vec![
                [(1, "HELLOWORLD".to_string())].into_iter().collect(),
                [(1, "HELLOTHERE".to_string())].into_iter().collect()
            ]
        );

        let commands = ZplParser::parse("^XA^HV1,10,+,,L^PQ2^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);
        let input = Cursor::new(b"+hi+hello");

        let verifications = check_host_verifications(input, &labels).await.unwrap();
        assert_eq!(
            verifications,
            vec![
                [(1, "hi".to_string())].into_iter().collect(),
                [(1, "hello".to_string())].into_iter().collect()
            ]
        );

        let commands = ZplParser::parse("^XA^HV1,10,+,,L^PQ2^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);
        let input = Cursor::new(b"+helloworld+hello");

        let verifications = check_host_verifications(input, &labels).await.unwrap();
        assert_eq!(
            verifications,
            vec![
                [(1, "helloworld".to_string())].into_iter().collect(),
                [(1, "hello".to_string())].into_iter().collect()
            ]
        );
    }

    #[tokio::test]
    async fn test_malformed_host_verifications() {
        let commands = ZplParser::parse("^XA^HV1,4,NAME[,],L^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);
        let input = Cursor::new(b"NAME[");

        let verifications = check_host_verifications(input, &labels).await;
        assert!(verifications.is_err());
    }

    #[tokio::test]
    async fn test_hanging_verification() {
        let _ = tracing_subscriber::fmt::try_init();

        let commands = ZplParser::parse("^XA^HV1,5,+,,L^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);

        let stream = tokio_stream::once(Ok::<_, std::io::Error>(Bytes::from_static(b"+hi")))
            .chain(tokio_stream::pending());
        let input = StreamReader::new(stream);

        let verifications = check_host_verifications(input, &labels).await.unwrap();
        assert_eq!(
            verifications,
            vec![[(1, "hi".to_string())].into_iter().collect()]
        );

        let commands = ZplParser::parse("^XA^HV1,5,+,,L^PQ2^XZ").unwrap();
        let labels = ZplParser::split_labels(&commands);

        let stream = tokio_stream::once(Ok::<_, std::io::Error>(Bytes::from_static(b"+hi")))
            .chain(tokio_stream::pending());
        let input = StreamReader::new(stream);

        let verifications = check_host_verifications(input, &labels).await;
        assert!(verifications.is_err());
    }
}
