//! Secret proxy — HTTPS MITM + HTTP rewrite for transparent credential injection.
//!
//! Listens on `0.0.0.0:8888`, handles three cases:
//!   1. Plain HTTP — rewrite auth headers, forward
//!   2. HTTPS CONNECT to allowed host — MITM with generated cert, rewrite headers
//!   3. HTTPS CONNECT to other hosts — blind TCP tunnel (no inspection)
//!
//! This module provides the proxy infrastructure: TCP listener, constants, and
//! the shared `ProxyState`. The full MITM/tunnel/HTTP-forward logic builds on
//! these foundations.

pub mod certs;
mod handler;
pub mod secrets;

use std::io;
use std::net::SocketAddr;
use std::path::Path;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use std::time::Duration;

use tokio::net::TcpListener;
use tracing::info;

use certs::{CaAuthority, CertCache};
use secrets::SecretsFile;

// ── Constants (must match Go proxy) ─────────────────────────────────

pub const PROXY_PORT: u16 = 8888;
pub const MAX_RESPONSE_BODY: u64 = 512 * 1024 * 1024; // 512 MB
pub const MAX_REQUEST_BODY: u64 = 64 * 1024 * 1024; // 64 MB
pub const MAX_CONCURRENT_CONNS: usize = 512;
pub const CONN_IDLE_TIMEOUT: Duration = Duration::from_secs(2 * 60);
pub const CONN_READ_TIMEOUT: Duration = Duration::from_secs(30);
pub const TUNNEL_TIMEOUT: Duration = Duration::from_secs(10 * 60);

// ── Hop-by-hop headers (RFC 2616 §13.5.1) ──────────────────────────

/// Headers that must not be forwarded by a proxy.
pub const HOP_BY_HOP_HEADERS: &[&str] = &[
    "connection",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "proxy-connection",
    "te",
    "trailer",
    "transfer-encoding",
    "upgrade",
];

// ── Shared proxy state ──────────────────────────────────────────────

/// Shared state for the proxy, accessible from all connection handlers.
pub struct ProxyState {
    pub secrets: Option<SecretsFile>,
    pub certs: CertCache,
    pub active_conns: AtomicI64,
    pub conn_semaphore: tokio::sync::Semaphore,
}

impl ProxyState {
    /// Initialize proxy state from the data directory.
    ///
    /// Loads secrets from `{data_dir}/secrets.json` (optional) and
    /// loads or generates the CA from `{data_dir}/proxy-ca/`.
    pub fn new(data_dir: &Path) -> io::Result<Self> {
        let secrets_path = data_dir.join("secrets.json");
        let ca_dir = data_dir.join("proxy-ca");

        let secrets = SecretsFile::load(&secrets_path)?;
        let ca = CaAuthority::load_or_generate(&ca_dir)?;
        let certs = CertCache::new(ca);

        Ok(Self {
            secrets,
            certs,
            active_conns: AtomicI64::new(0),
            conn_semaphore: tokio::sync::Semaphore::new(MAX_CONCURRENT_CONNS),
        })
    }

    /// Check if a host is in any secret's allowed_hosts list.
    pub fn host_allowed(&self, host: &str) -> bool {
        self.secrets
            .as_ref()
            .map(|s| s.host_allowed(host))
            .unwrap_or(false)
    }

    /// Current number of active connections.
    pub fn active_connections(&self) -> i64 {
        self.active_conns.load(Ordering::Relaxed)
    }

    /// Number of configured secrets.
    pub fn secret_count(&self) -> usize {
        self.secrets.as_ref().map(|s| s.secrets.len()).unwrap_or(0)
    }
}

/// Start the proxy TCP listener on `0.0.0.0:8888`.
///
/// Returns the listener and the bound address. The caller is responsible for
/// spawning the accept loop (which will be implemented in a follow-up task
/// that adds the full MITM/tunnel/HTTP-forward logic).
pub async fn bind_listener() -> io::Result<(TcpListener, SocketAddr)> {
    let addr: SocketAddr = ([0, 0, 0, 0], PROXY_PORT).into();
    let listener = TcpListener::bind(addr).await?;
    let local_addr = listener.local_addr()?;
    info!(addr = %local_addr, "secret proxy listening");
    Ok((listener, local_addr))
}

/// Start the secret proxy as a tokio task.
///
/// This is the main entry point called from `squashd` startup. It binds the
/// listener and spawns the accept loop. Returns the bound address.
pub async fn start(state: Arc<ProxyState>) -> io::Result<SocketAddr> {
    let (listener, addr) = bind_listener().await?;

    info!(
        addr = %addr,
        secrets = state.secret_count(),
        "sq-secret-proxy starting"
    );

    tokio::spawn(accept_loop(listener, state));

    Ok(addr)
}

/// Accept loop — accepts TCP connections and spawns a handler task for each.
///
/// The per-connection handler (handle_connection) is a stub that will be
/// fleshed out in the follow-up MITM/tunnel task.
async fn accept_loop(listener: TcpListener, state: Arc<ProxyState>) {
    loop {
        let (stream, peer) = match listener.accept().await {
            Ok(conn) => conn,
            Err(e) => {
                tracing::error!(error = %e, "accept error");
                continue;
            }
        };

        let state = Arc::clone(&state);
        tokio::spawn(async move {
            handle_connection(stream, peer, state).await;
        });
    }
}

/// Per-connection handler.
///
/// Acquires a semaphore permit (limiting to MAX_CONCURRENT_CONNS), then
/// dispatches to the handler module which parses the HTTP request line
/// and routes to CONNECT (MITM or blind tunnel) or plain HTTP forwarding.
async fn handle_connection(
    stream: tokio::net::TcpStream,
    peer: SocketAddr,
    state: Arc<ProxyState>,
) {
    // Acquire semaphore permit (limit concurrent connections)
    let _permit = match state.conn_semaphore.try_acquire() {
        Ok(permit) => permit,
        Err(_) => {
            tracing::warn!(peer = %peer, "too many connections, rejecting");
            return;
        }
    };

    state.active_conns.fetch_add(1, Ordering::Relaxed);

    handler::handle_connection(stream, peer, state.clone()).await;

    state.active_conns.fetch_sub(1, Ordering::Relaxed);
}
