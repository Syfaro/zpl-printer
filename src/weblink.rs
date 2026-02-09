use std::{collections::HashSet, net::SocketAddr, str::FromStr, sync::Arc, time::Duration};

use age::{
    secrecy::{ExposeSecret, SecretString},
    x25519::Identity,
};
use axum::{
    Extension,
    extract::{
        Request, State, WebSocketUpgrade,
        ws::{Message, WebSocket},
    },
    middleware::{AddExtension, Next},
    response::{IntoResponse, Response},
};
use axum_server::{accept::Accept, tls_rustls::RustlsAcceptor};
use bytes::Bytes;
use chrono::{NaiveDateTime, TimeZone};
use clap::{Args, Parser};
use futures_util::future::BoxFuture;
use migration::Expr;
use rcgen::{
    Certificate, CertificateParams, DistinguishedName, DnType, ExtendedKeyUsagePurpose, Issuer,
    KeyPair, KeyUsagePurpose, PKCS_RSA_SHA256, RsaKeySize, SerialNumber,
};
use reqwest::StatusCode;
use rustls::{
    RootCertStore,
    pki_types::{CertificateDer, PrivateKeyDer, pem::PemObject},
};
use sea_orm::{
    ActiveModelTrait, ActiveValue::Set, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter,
    QueryOrder,
};
use serde::{Deserialize, Serialize};
use time::OffsetDateTime;
use tokio::{
    io::{AsyncRead, AsyncWrite},
    sync::{broadcast, mpsc, oneshot},
    time::timeout,
};
use tokio_rustls::server::TlsStream;
use tokio_util::sync::CancellationToken;
use tower::Layer;
use tracing::{Instrument, debug, error, info, instrument, trace, warn};
use uuid::Uuid;
use x509_parser::prelude::{FromDer, X509Certificate};

use crate::{
    AppError,
    entities::{alert, printer, web_link_certificate},
    transport::{ActionWithSender, TransportAction},
    web::AppState,
};

const SERIAL_NUMBER_LEN: usize = 20;

#[derive(Clone, Copy, Debug)]
enum CertificateType {
    Root,
    Server,
    Printer,
}

impl std::fmt::Display for CertificateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "root"),
            Self::Server => write!(f, "server"),
            Self::Printer => write!(f, "printer"),
        }
    }
}

#[derive(Args)]
pub struct WebLinkConfig {
    /// If weblink support should be enabled.
    #[clap(
        long = "enable-weblink",
        env = "ENABLE_WEBLINK",
        requires = "root_certificate_id",
        requires = "root_encryption_key",
        requires = "server_names"
    )]
    pub enable: bool,
    /// Address for weblink server, if a separate address is desired.
    ///
    /// If this is not specified the main listener will require TLS.
    #[clap(long, env)]
    pub weblink_address: Option<SocketAddr>,
    /// The ID of the root certificate this server should use.
    ///
    /// A root certificate can be generated with the `generate-ca`
    /// subcommand.
    #[clap(long, env)]
    pub root_certificate_id: Option<Uuid>,
    /// The encryption key for certificates.
    ///
    /// A secret can be generated with the `generate-secret` subcommand.
    #[clap(long, env)]
    root_encryption_key: Option<SecretString>,
    /// Server names to use when generating a server certificate.
    ///
    /// This must include the name of the server that the printer will use to
    /// connect!
    #[clap(long, env)]
    server_names: Option<Vec<String>>,
    /// Validity period of server certificates.
    #[clap(long, env, default_value = "365")]
    server_valid_days: i64,
    /// How many days before expiration the server will issue a new certificate.
    #[clap(long, env, default_value = "30")]
    server_renew_threshold: i64,
}

impl WebLinkConfig {
    pub fn identity(&self) -> eyre::Result<Identity> {
        Identity::from_str(
            self.root_encryption_key
                .as_ref()
                .ok_or_else(|| eyre::eyre!("encryption key must be present"))?
                .expose_secret(),
        )
        .map_err(|err| eyre::eyre!("{err}"))
    }
}

#[derive(Args)]
pub struct CaConfig {
    /// Database DSN.
    #[clap(long, env)]
    database_url: String,
    /// The encryption key for certificates.
    #[clap(long, env)]
    root_encryption_key: SecretString,
}

impl CaConfig {
    fn identity(&self) -> eyre::Result<Identity> {
        Identity::from_str(self.root_encryption_key.expose_secret())
            .map_err(|err| eyre::eyre!("{err}"))
    }
}

#[derive(Parser)]
pub struct GenerateCaConfig {
    /// Certificate organization name.
    #[clap(long, env)]
    organization_name: String,
    /// Certificate common name.
    #[clap(long, env)]
    common_name: String,
    /// Validity period in days.
    #[clap(long, env, default_value = "3650")]
    valid_days: i64,

    #[clap(flatten)]
    shared: CaConfig,
}

pub async fn generate_ca(ca_config: GenerateCaConfig) -> eyre::Result<web_link_certificate::Model> {
    let db = super::prepare_db(&ca_config.shared.database_url).await?;

    let mut params = CertificateParams::default();

    let mut dn = DistinguishedName::new();
    dn.push(DnType::OrganizationName, ca_config.organization_name);
    dn.push(DnType::CommonName, ca_config.common_name);
    params.distinguished_name = dn;

    let (created_at, expires_at) = validity_period(ca_config.valid_days);
    params.not_before = created_at;
    params.not_after = expires_at;

    params.is_ca = rcgen::IsCa::Ca(rcgen::BasicConstraints::Unconstrained);

    params.key_usages = vec![
        rcgen::KeyUsagePurpose::KeyCertSign,
        rcgen::KeyUsagePurpose::CrlSign,
    ];

    let sn = generate_serial_number();
    params.serial_number = Some(sn.clone());

    let key_pair = KeyPair::generate_rsa_for(&PKCS_RSA_SHA256, RsaKeySize::_2048)?;
    let cert = params.self_signed(&key_pair)?;

    let cert_pem = cert.pem();
    let key_pem = key_pair.serialize_pem();

    let identity = ca_config.shared.identity()?;
    let encrypted_pem = age::encrypt(&identity.to_public(), key_pem.as_bytes())?;

    let cert = web_link_certificate::ActiveModel {
        id: Set(Uuid::now_v7()),
        certificate_type: Set(CertificateType::Root.to_string()),
        serial_number: Set(sn.to_bytes()),
        public_key: Set(cert_pem),
        private_key: Set(Some(encrypted_pem)),
        created_at: Set(offset_to_chrono(created_at)),
        expires_at: Set(offset_to_chrono(expires_at)),
        ..Default::default()
    };

    let cert = cert.insert(&db).await?;
    Ok(cert)
}

pub fn get_root_store(roots: &[&web_link_certificate::Model]) -> eyre::Result<RootCertStore> {
    let mut store = RootCertStore::empty();
    for root in roots {
        let cert = CertificateDer::from_pem_slice(root.public_key.as_bytes())?;
        store.add(cert)?;
    }
    Ok(store)
}

pub async fn fetch_or_generate_server_cert(
    db: &DatabaseConnection,
    config: &WebLinkConfig,
) -> eyre::Result<(Vec<CertificateDer<'static>>, PrivateKeyDer<'static>)> {
    let root_ca = get_root_ca(
        db,
        config
            .root_certificate_id
            .expect("root certificate id must be present"),
    )
    .await?
    .ok_or_else(|| eyre::eyre!("root ca must already exist"))?;

    let server_names = config
        .server_names
        .as_ref()
        .expect("server names must be present");
    let existing_cert =
        get_server_cert(db, root_ca.id, config.server_renew_threshold, server_names).await?;

    let (server_cert, server_key_pair) = if let Some(server_cert) = existing_cert {
        debug!("found existing server certificate");
        let identity = config.identity()?;
        let server_private_key = age::decrypt(
            &identity,
            &server_cert
                .private_key
                .expect("server private key should always exist"),
        )?;
        let server_private_key = String::from_utf8(server_private_key)?;
        (
            CertificateDer::from_pem_slice(server_cert.public_key.as_bytes())?,
            KeyPair::from_pem(&server_private_key)?,
        )
    } else {
        info!("generating new server certificate");
        generate_server_cert(db, config, &root_ca, server_names).await?
    };

    let server_chain = vec![server_cert];
    let server_key = PrivateKeyDer::try_from(server_key_pair.serialized_der().to_vec())
        .expect("key pair should always convert to private key");

    Ok((server_chain, server_key))
}

pub async fn get_root_ca(
    db: &DatabaseConnection,
    id: Uuid,
) -> eyre::Result<Option<web_link_certificate::Model>> {
    let root_ca = web_link_certificate::Entity::find()
        .filter(web_link_certificate::Column::Id.eq(id).and(
            web_link_certificate::Column::CertificateType.eq(CertificateType::Root.to_string()),
        ))
        .one(db)
        .await?;

    Ok(root_ca)
}

pub async fn generate_printer_cert(
    state: &AppState,
    printer: &printer::Model,
) -> eyre::Result<(String, KeyPair, OffsetDateTime)> {
    let weblink_state = state.weblink.as_ref().unwrap();

    let mut params = CertificateParams::default();
    params.distinguished_name.push(
        DnType::CommonName,
        printer.unique_id.as_deref().unwrap_or(&printer.name),
    );

    let (created_at, expires_at) = validity_period(365);
    params.not_before = created_at;
    params.not_after = expires_at;

    params.key_usages.push(KeyUsagePurpose::DigitalSignature);
    params
        .extended_key_usages
        .push(ExtendedKeyUsagePurpose::ClientAuth);

    let sn = generate_serial_number();
    params.serial_number = Some(sn.clone());

    let key_pair = KeyPair::generate_rsa_for(&PKCS_RSA_SHA256, RsaKeySize::_2048)?;
    let printer_cert = issue_cert(
        &weblink_state.root_encryption_identity,
        &weblink_state.root_ca,
        &key_pair,
        params,
    )?;

    let cert_pem = printer_cert.pem();

    let cert = web_link_certificate::ActiveModel {
        id: Set(Uuid::now_v7()),
        certificate_type: Set(CertificateType::Printer.to_string()),
        issuer_id: Set(Some(weblink_state.root_ca.id)),
        serial_number: Set(sn.to_bytes()),
        public_key: Set(cert_pem.clone()),
        private_key: Set(None),
        printer_id: Set(Some(printer.id)),
        created_at: Set(offset_to_chrono(created_at)),
        expires_at: Set(offset_to_chrono(expires_at)),
        ..Default::default()
    };

    cert.insert(&state.db).await?;

    Ok((cert_pem, key_pair, created_at))
}

async fn get_server_cert(
    db: &DatabaseConnection,
    root_id: Uuid,
    renew_threshold: i64,
    alt_names: &[String],
) -> eyre::Result<Option<web_link_certificate::Model>> {
    let server_certs = web_link_certificate::Entity::find()
        .filter(
            web_link_certificate::Column::IssuerId
                .eq(root_id)
                .and(
                    web_link_certificate::Column::CertificateType
                        .eq(CertificateType::Server.to_string()),
                )
                .and(web_link_certificate::Column::RevokedAt.is_null())
                .and(
                    web_link_certificate::Column::ExpiresAt
                        .gt(chrono::Utc::now() + chrono::Duration::days(renew_threshold)),
                ),
        )
        .order_by_desc(web_link_certificate::Column::ExpiresAt)
        .all(db)
        .await?;

    let alt_names: HashSet<&str> = alt_names.iter().map(|name| name.as_str()).collect();

    let valid_cert = server_certs.into_iter().find(|cert| {
        let Ok(der) = CertificateDer::from_pem_slice(cert.public_key.as_bytes()) else {
            return false;
        };

        let Ok((_, cert)) = X509Certificate::from_der(&der) else {
            return false;
        };

        let Ok(Some(san)) = cert.subject_alternative_name() else {
            return false;
        };

        let cert_names: HashSet<&str> = san
            .value
            .general_names
            .iter()
            .filter_map(|name| match name {
                x509_parser::prelude::GeneralName::DNSName(name) => Some(*name),
                _ => None,
            })
            .collect();

        alt_names.is_superset(&cert_names)
    });

    Ok(valid_cert)
}

async fn generate_server_cert(
    db: &DatabaseConnection,
    config: &WebLinkConfig,
    root_ca: &web_link_certificate::Model,
    server_names: &[String],
) -> eyre::Result<(CertificateDer<'static>, KeyPair)> {
    let mut params = CertificateParams::new(server_names)?;
    params.use_authority_key_identifier_extension = true;
    params.key_usages.push(KeyUsagePurpose::DigitalSignature);
    params
        .extended_key_usages
        .push(ExtendedKeyUsagePurpose::ServerAuth);
    params.distinguished_name.push(
        DnType::CommonName,
        server_names
            .first()
            .expect("at least one server name must exist"),
    );

    let (created_at, expires_at) = validity_period(config.server_valid_days);
    params.not_before = created_at;
    params.not_after = expires_at;

    let sn = generate_serial_number();
    params.serial_number = Some(sn.clone());

    let identity = config.identity()?;
    let key_pair = KeyPair::generate_rsa_for(&PKCS_RSA_SHA256, RsaKeySize::_2048)?;
    let server_cert = issue_cert(&identity, root_ca, &key_pair, params)?;

    let cert_pem = server_cert.pem();
    let secret_key = age::encrypt(&identity.to_public(), key_pair.serialize_pem().as_bytes())?;

    let cert = web_link_certificate::ActiveModel {
        id: Set(Uuid::now_v7()),
        certificate_type: Set(CertificateType::Server.to_string()),
        issuer_id: Set(Some(root_ca.id)),
        serial_number: Set(sn.to_bytes()),
        public_key: Set(cert_pem.clone()),
        private_key: Set(Some(secret_key)),
        created_at: Set(offset_to_chrono(created_at)),
        expires_at: Set(offset_to_chrono(expires_at)),
        ..Default::default()
    };
    cert.insert(db).await?;

    Ok((server_cert.der().to_owned(), key_pair))
}

fn validity_period(days: i64) -> (OffsetDateTime, OffsetDateTime) {
    let created_at = OffsetDateTime::now_utc();
    let expires_at = created_at + time::Duration::days(days);
    (created_at, expires_at)
}

fn generate_serial_number() -> SerialNumber {
    let mut sn = [0u8; SERIAL_NUMBER_LEN];
    rand::fill(&mut sn);
    sn[0] &= 0b01111111;
    SerialNumber::from_slice(&sn)
}

fn offset_to_chrono(input: OffsetDateTime) -> chrono::DateTime<chrono::FixedOffset> {
    chrono::Utc
        .timestamp_opt(input.unix_timestamp(), 0)
        .unwrap()
        .into()
}

fn issue_cert(
    identity: &Identity,
    root_ca: &web_link_certificate::Model,
    key_pair: &KeyPair,
    params: CertificateParams,
) -> eyre::Result<Certificate> {
    let ca_private_key = age::decrypt(
        identity,
        root_ca
            .private_key
            .as_ref()
            .expect("root should always have private key"),
    )?;
    let ca_private_key = String::from_utf8(ca_private_key)?;
    let ca_key_pair = KeyPair::from_pem(&ca_private_key)?;

    let issuer = Issuer::from_ca_cert_pem(&root_ca.public_key, ca_key_pair)?;
    let cert = params.signed_by(&key_pair, &issuer)?;
    Ok(cert)
}

#[derive(Clone, Debug)]
pub struct TlsData {
    certificates: Vec<CertificateDer<'static>>,
}

#[derive(Clone)]
pub struct AcceptorWithCerts {
    inner: RustlsAcceptor,
}

impl AcceptorWithCerts {
    pub fn new(inner: RustlsAcceptor) -> Self {
        Self { inner }
    }
}

impl<I, S> Accept<I, S> for AcceptorWithCerts
where
    I: AsyncRead + AsyncWrite + Unpin + Send + 'static,
    S: Send + 'static,
{
    type Stream = TlsStream<I>;
    type Service = AddExtension<S, TlsData>;
    type Future = BoxFuture<'static, std::io::Result<(Self::Stream, Self::Service)>>;

    fn accept(&self, stream: I, service: S) -> Self::Future {
        let acceptor = self.inner.clone();

        Box::pin(async move {
            let (stream, service) = acceptor.accept(stream, service).await?;
            let server_conn = stream.get_ref().1;

            let certificates: Vec<_> = server_conn.peer_certificates().unwrap_or_default().to_vec();
            trace!(len = certificates.len(), "got certificates from request");
            let service = Extension(TlsData { certificates }).layer(service);

            Ok((stream, service))
        })
    }
}

pub async fn require_printer_cert(
    Extension(tls_data): Extension<TlsData>,
    State(state): State<Arc<AppState>>,
    mut request: Request,
    next: Next,
) -> Result<Response, AppError> {
    let serial_numbers: Vec<_> = tls_data
        .certificates
        .into_iter()
        .flat_map(|cert| {
            X509Certificate::from_der(&cert)
                .ok()
                .map(|(_, cert)| cert.raw_serial().to_vec())
        })
        .collect();
    trace!("got serial numbers: {serial_numbers:?}");

    let cert = web_link_certificate::Entity::find()
        .filter(
            web_link_certificate::Column::SerialNumber
                .is_in(serial_numbers)
                .and(
                    web_link_certificate::Column::CertificateType
                        .eq(CertificateType::Printer.to_string()),
                )
                .and(web_link_certificate::Column::RevokedAt.is_null())
                .and(web_link_certificate::Column::ExpiresAt.gt(chrono::Utc::now())),
        )
        .one(&state.db)
        .await?;

    let Some(cert) = cert else {
        return Ok((StatusCode::FORBIDDEN).into_response());
    };

    debug!(id = %cert.id, printer_id = %cert.printer_id.unwrap(), "got known certificate");
    request.extensions_mut().insert(cert);

    Ok(next.run(request).await)
}

#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum ZebraMessage {
    Alert {
        alert: ZebraAlert,
    },
    Discovery {
        discovery_b64: String,
    },
    Channel {
        channel_id: String,
        channel_name: ZebraChannel,
        unique_id: String,
    },
    Other(serde_json::Value),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ZebraChannel {
    Main,
    #[serde(rename = "v1.config.zebra.com")]
    ConfigV1,
    #[serde(rename = "v1.raw.zebra.com")]
    RawV1,
}

impl std::fmt::Display for ZebraChannel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ZebraChannel::Main => write!(f, "main"),
            ZebraChannel::ConfigV1 => write!(f, "config"),
            ZebraChannel::RawV1 => write!(f, "raw"),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Serialize, Deserialize)]
struct ZebraAlert {
    unique_id: String,
    #[serde(with = "basic_format")]
    time_stamp: NaiveDateTime,
    type_id: String,
    condition_id: String,
    condition_state: String,
    #[serde(rename = "type")]
    alert_type: String,
    condition: String,
}

mod basic_format {
    use chrono::NaiveDateTime;
    use serde::{Deserialize, Deserializer, Serializer};

    const FORMAT: &str = "%Y-%m-%d %H:%M:%S";

    pub fn serialize<S>(date: &NaiveDateTime, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = date.format(FORMAT);
        serializer.serialize_str(&s.to_string())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<NaiveDateTime, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let dt = NaiveDateTime::parse_from_str(&s, FORMAT).map_err(serde::de::Error::custom)?;
        Ok(dt)
    }
}

pub async fn endpoint(
    ws: WebSocketUpgrade,
    State(state): State<Arc<AppState>>,
    Extension(cert): Extension<web_link_certificate::Model>,
) -> Response {
    ws.protocols(["v1.weblink.zebra.com"])
        .on_upgrade(move |socket: WebSocket| handle_socket(socket, cert, state).in_current_span())
}

#[instrument(skip_all, fields(cert_id = %cert.id, unique_id))]
async fn handle_socket(socket: WebSocket, cert: web_link_certificate::Model, state: Arc<AppState>) {
    debug!("starting connection");

    let weblink_state = state.weblink.as_ref().unwrap();

    let Some(printer) = printer::Entity::find_by_id(cert.printer_id.unwrap())
        .one(&state.db)
        .await
        .ok()
        .flatten()
    else {
        error!("could not find printer");
        WebSocketConnection::close_socket(socket).await;
        return;
    };

    let mut conn = WebSocketConnection::new(
        state.clone(),
        state.token.child_token(),
        cert,
        printer,
        socket,
    )
    .await;

    if let Some(unique_id) = conn.printer.unique_id.as_deref() {
        tracing::Span::current().record("unique_id", unique_id);
    }

    let channel_type = match conn.wait_for_channel_type().await {
        Ok(channel_type) => channel_type,
        Err(err) => {
            error!("error while waiting for channel type: {err}");
            conn.close().await;
            return;
        }
    };

    let (actions, data_tx) = match weblink_state
        .transport
        .prepare((conn.printer.id, channel_type), conn.token.clone())
        .await
    {
        Ok(transport) => transport,
        Err(err) => {
            error!("could not create transport: {err}");
            conn.close().await;
            return;
        }
    };

    weblink_state.tasks.spawn(
        async move {
            if let Err(err) = conn.process_channel(channel_type, actions, data_tx).await
                && !conn.token.is_cancelled()
            {
                error!("processing channel failed: {err}");
            }

            conn.close().await;
        }
        .in_current_span(),
    );
}

struct WebSocketConnection {
    state: Arc<AppState>,
    token: CancellationToken,
    cert: web_link_certificate::Model,
    printer: printer::Model,
    socket: WebSocket,
}

impl WebSocketConnection {
    async fn new(
        state: Arc<AppState>,
        token: CancellationToken,
        cert: web_link_certificate::Model,
        printer: printer::Model,
        socket: WebSocket,
    ) -> Self {
        Self {
            state,
            token,
            cert,
            printer,
            socket,
        }
    }

    async fn close_socket(mut socket: WebSocket) {
        if socket.send(Message::Close(None)).await.is_ok() {
            let _ = tokio::time::timeout(Duration::from_secs(30), async move {
                while let Some(msg) = socket.recv().await {
                    if !matches!(msg, Ok(Message::Close(_)) | Err(_)) {
                        warn!("got message other than close while shutting down: {msg:?}");
                    }
                }
            })
            .await;
        }
    }

    async fn close(self) {
        Self::close_socket(self.socket).await;
    }

    async fn wait_for_channel_type(&mut self) -> eyre::Result<ZebraChannel> {
        let Some(message) = timeout(Duration::from_secs(5), self.socket.recv())
            .await?
            .transpose()?
        else {
            eyre::bail!("did not receive initial message");
        };

        trace!("got first message: {message:?}");

        let Message::Binary(data) = message else {
            eyre::bail!("message was not binary");
        };

        let message = serde_json::from_slice::<ZebraMessage>(&data)?;

        let channel = match &message {
            ZebraMessage::Discovery { .. } => {
                self.ping().await?;

                let open_payload = serde_json::json!({
                    "open": serde_plain::to_string(&ZebraChannel::RawV1).unwrap(),
                });
                self.socket
                    .send(Message::Binary(serde_json::to_vec(&open_payload)?.into()))
                    .await?;

                let configure_alert_payload = serde_json::json!({
                    "configure_alert": "ALL MESSAGES,SDK,Y,Y,,,N"
                });
                self.socket
                    .send(Message::Binary(
                        serde_json::to_vec(&configure_alert_payload)?.into(),
                    ))
                    .await?;

                ZebraChannel::Main
            }
            ZebraMessage::Channel {
                channel_name,
                unique_id,
                ..
            } => {
                match self.printer.unique_id.as_ref() {
                    Some(id) if id != unique_id => warn!(
                        expected = id,
                        actual = unique_id,
                        "connected printer had different unique id"
                    ),
                    None => warn!("printer does not have unique id set"),
                    _ => (),
                }

                *channel_name
            }
            other => {
                eyre::bail!("unexpected first message type: {other:?}");
            }
        };

        Ok(channel)
    }

    async fn process_channel(
        &mut self,
        channel_type: ZebraChannel,
        mut actions: mpsc::Receiver<ActionWithSender>,
        data_tx: broadcast::Sender<Bytes>,
    ) -> eyre::Result<()> {
        loop {
            tokio::select! {
                _ = self.token.cancelled() => {
                    info!("task cancelled");
                    return Ok(());
                }

                msg = self.socket.recv() => {
                    self.process_message(channel_type, msg, data_tx.clone()).await?;
                }

                action = actions.recv() => {
                    if self.process_action(action).await? {
                        info!("action asked for disconnect");
                        return Ok(());
                    }
                }
            }
        }
    }

    #[instrument(skip(self, msg, data_tx))]
    async fn process_message(
        &mut self,
        channel_type: ZebraChannel,
        msg: Option<Result<Message, axum::Error>>,
        data_tx: broadcast::Sender<Bytes>,
    ) -> eyre::Result<()> {
        match msg
            .transpose()?
            .ok_or_else(|| eyre::eyre!("message channel closed"))?
        {
            Message::Binary(data) => {
                if let Ok(message) = serde_json::from_slice::<ZebraMessage>(&data) {
                    debug!("{message:?}");

                    if let ZebraMessage::Alert { alert } = message {
                        self.state
                            .alert_handler
                            .process(alert::ActiveModel {
                                alert_type: Set(alert.alert_type),
                                alert_message: Set(alert.condition),
                                printer_timestamp: Set(alert.time_stamp),
                                unique_id: Set(alert.unique_id),
                                ..Default::default()
                            })
                            .await?;
                    }
                } else {
                    trace!("{data:?}");
                };

                // It's okay if nothing is listening.
                let _ = data_tx.send(data);

                Ok(())
            }
            Message::Ping(_) if channel_type == ZebraChannel::Main => {
                trace!("got ping on main channel");
                self.ping().await?;
                Ok(())
            }
            other => {
                trace!("got other message: {other:?}");
                Ok(())
            }
        }
    }

    #[instrument(skip_all)]
    async fn process_action(
        &mut self,
        action: Option<(TransportAction, oneshot::Sender<eyre::Result<()>>)>,
    ) -> eyre::Result<bool> {
        let Some((action, tx)) = action else {
            eyre::bail!("action channel closed");
        };

        trace!("handling transport action: {action:?}");

        match action {
            TransportAction::Disconnect => {
                let _ = tx.send(Ok(()));
                Ok(true)
            }

            TransportAction::Write(payload) => {
                match self.socket.send(Message::Binary(payload)).await {
                    Ok(_) => {
                        debug!("sent payload");
                        match tx.send(Ok(())) {
                            Ok(_) => Ok(false),
                            Err(_) => eyre::bail!("could not send ok"),
                        }
                    }
                    Err(err) => {
                        error!("error sending payload: {err}");
                        let _ = tx.send(Err(eyre::eyre!("failed to send message: {err}")));
                        Err(err.into())
                    }
                }
            }
        }
    }

    #[instrument(skip_all)]
    async fn ping(&mut self) -> eyre::Result<()> {
        web_link_certificate::Entity::update_many()
            .col_expr(
                web_link_certificate::Column::LastPingAt,
                Expr::current_timestamp().into(),
            )
            .filter(web_link_certificate::Column::Id.eq(self.cert.id))
            .exec(&self.state.db)
            .await?;

        Ok(())
    }
}
