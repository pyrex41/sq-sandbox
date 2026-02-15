//! Proxy connection handler — HTTP forwarding, CONNECT MITM, and blind tunneling.
//!
//! Implements the three proxy modes:
//!   1. Plain HTTP — parse request, rewrite auth headers, strip hop-by-hop, forward
//!   2. HTTPS CONNECT to allowed host — TLS MITM with generated cert, rewrite headers
//!   3. HTTPS CONNECT to other hosts — blind bidirectional TCP tunnel

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tokio::time::timeout;
use tokio_rustls::TlsAcceptor;
use tracing::{debug, info, warn};

use rustls::server::{ClientHello, ResolvesServerCert};
use rustls::sign::CertifiedKey;

use super::secrets::is_replaceable_header;
use super::{
    ProxyState, CONN_IDLE_TIMEOUT, CONN_READ_TIMEOUT, HOP_BY_HOP_HEADERS, MAX_REQUEST_BODY,
    MAX_RESPONSE_BODY, TUNNEL_TIMEOUT,
};

// ── Cert resolver for MITM TLS ─────────────────────────────────────

/// Simple cert resolver that always returns the same pre-generated certificate.
struct CertResolver(Arc<CertifiedKey>);

impl ResolvesServerCert for CertResolver {
    fn resolve(&self, _client_hello: ClientHello<'_>) -> Option<Arc<CertifiedKey>> {
        Some(Arc::clone(&self.0))
    }
}

impl std::fmt::Debug for CertResolver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CertResolver").finish()
    }
}

// ── HTTP request line parsing ───────────────────────────────────────

/// A parsed HTTP request from the raw TCP stream.
struct RawRequest {
    method: String,
    uri: String,
    #[allow(dead_code)]
    version: String,
    headers: Vec<(String, String)>,
}

/// Read an HTTP/1.x request (request-line + headers) from a buffered reader.
///
/// Returns the parsed request. The reader's position will be just past the
/// `\r\n\r\n` header terminator.
async fn read_http_request<R: tokio::io::AsyncBufRead + Unpin>(
    reader: &mut R,
) -> io::Result<RawRequest> {
    // Read request line
    let mut request_line = String::new();
    let n = reader.read_line(&mut request_line).await?;
    if n == 0 {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "empty request"));
    }

    let request_line = request_line.trim_end();
    let parts: Vec<&str> = request_line.splitn(3, ' ').collect();
    if parts.len() < 3 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("malformed request line: {request_line}"),
        ));
    }

    let method = parts[0].to_owned();
    let uri = parts[1].to_owned();
    let version = parts[2].to_owned();

    // Read headers until empty line
    let mut headers = Vec::new();
    loop {
        let mut line = String::new();
        let n = reader.read_line(&mut line).await?;
        if n == 0 {
            break;
        }
        let trimmed = line.trim_end_matches(|c| c == '\r' || c == '\n');
        if trimmed.is_empty() {
            break;
        }
        if let Some((name, value)) = trimmed.split_once(':') {
            headers.push((name.trim().to_owned(), value.trim().to_owned()));
        }
    }

    Ok(RawRequest {
        method,
        uri,
        version,
        headers,
    })
}

// ── Header helpers ──────────────────────────────────────────────────

/// Find the value of a header by name (case-insensitive).
fn find_header<'a>(headers: &'a [(String, String)], name: &str) -> Option<&'a str> {
    let lower = name.to_ascii_lowercase();
    headers
        .iter()
        .find(|(n, _)| n.to_ascii_lowercase() == lower)
        .map(|(_, v)| v.as_str())
}

/// Get the Content-Length value from headers, or 0.
fn content_length(headers: &[(String, String)]) -> u64 {
    find_header(headers, "content-length")
        .and_then(|v| v.parse().ok())
        .unwrap_or(0)
}

/// Strip hop-by-hop headers from the list.
fn strip_hop_by_hop(headers: &mut Vec<(String, String)>) {
    headers.retain(|(name, _)| {
        let lower = name.to_ascii_lowercase();
        !HOP_BY_HOP_HEADERS.iter().any(|&h| h == lower)
    });
}

/// Replace secret placeholders in replaceable headers.
fn replace_headers(
    headers: &mut Vec<(String, String)>,
    host: &str,
    state: &ProxyState,
) {
    let secrets = match &state.secrets {
        Some(s) => s,
        None => return,
    };

    for (name, value) in headers.iter_mut() {
        if !is_replaceable_header(name) {
            continue;
        }
        let (new_value, _replaced) = secrets.replace_in_value(value, host);
        *value = new_value;
    }
}

/// Format headers as HTTP/1.1 header block string.
fn format_headers(headers: &[(String, String)]) -> String {
    let mut s = String::new();
    for (name, value) in headers {
        s.push_str(name);
        s.push_str(": ");
        s.push_str(value);
        s.push_str("\r\n");
    }
    s
}

/// Extract host from a CONNECT target or absolute URI.
/// For CONNECT: "example.com:443" → ("example.com", "example.com:443")
/// For HTTP: "http://example.com/path" → ("example.com", "example.com:80")
fn parse_connect_host(uri: &str) -> (String, String) {
    // Strip port to get bare host
    let host_port = uri.to_owned();
    let host = if let Some(idx) = uri.rfind(':') {
        // Check if this is an IPv6 address
        if uri.contains('[') {
            // IPv6: [::1]:443
            if let Some(bracket_end) = uri.find(']') {
                if idx > bracket_end {
                    uri[..idx].to_owned()
                } else {
                    uri.to_owned()
                }
            } else {
                uri.to_owned()
            }
        } else {
            // Regular host:port
            uri[..idx].to_owned()
        }
    } else {
        uri.to_owned()
    };

    (host, host_port)
}

/// Ensure a host:port string has a port, defaulting to 443.
fn ensure_port(host_port: &str) -> String {
    if host_port.contains(':') {
        // Could be host:port already — try to verify it's actually a port
        if let Some(after_colon) = host_port.rsplit(':').next() {
            if after_colon.parse::<u16>().is_ok() {
                return host_port.to_owned();
            }
        }
    }
    format!("{host_port}:443")
}

// ── Main connection handler ─────────────────────────────────────────

/// Handle a single proxy connection.
///
/// Reads the HTTP request line to determine the mode:
/// - CONNECT → dispatch to MITM or tunnel based on host allowlist
/// - Other methods → forward as plain HTTP
pub async fn handle_connection(
    stream: TcpStream,
    peer: SocketAddr,
    state: Arc<ProxyState>,
) {
    let mut reader = BufReader::new(stream);

    // Read the first HTTP request
    let req = match timeout(CONN_READ_TIMEOUT, read_http_request(&mut reader)).await {
        Ok(Ok(req)) => req,
        Ok(Err(e)) => {
            debug!(peer = %peer, error = %e, "failed to read request");
            return;
        }
        Err(_) => {
            debug!(peer = %peer, "read timeout");
            return;
        }
    };

    if req.method.eq_ignore_ascii_case("CONNECT") {
        handle_connect(reader, peer, &req, &state).await;
    } else {
        handle_http(reader, peer, &req, &state).await;
    }
}

// ── Plain HTTP forwarding ───────────────────────────────────────────

/// Forward a plain HTTP request with header replacement.
async fn handle_http(
    mut reader: BufReader<TcpStream>,
    peer: SocketAddr,
    req: &RawRequest,
    state: &ProxyState,
) {
    // Extract hostname from the URI
    let host = extract_host_from_uri(&req.uri);
    debug!(peer = %peer, host = %host, method = %req.method, "HTTP forward");

    let mut headers = req.headers.clone();
    replace_headers(&mut headers, &host, state);
    strip_hop_by_hop(&mut headers);

    // Read request body (limited)
    let body_len = content_length(&req.headers).min(MAX_REQUEST_BODY);
    let mut body = vec![0u8; body_len as usize];
    if body_len > 0 {
        if let Err(e) = reader.read_exact(&mut body).await {
            debug!(peer = %peer, error = %e, "failed to read request body");
            let _ = write_error_response(reader.get_mut(), 400, "Bad Request").await;
            return;
        }
    }

    // Build reqwest request
    let client = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .unwrap();

    let method = match req.method.to_ascii_uppercase().as_str() {
        "GET" => reqwest::Method::GET,
        "POST" => reqwest::Method::POST,
        "PUT" => reqwest::Method::PUT,
        "DELETE" => reqwest::Method::DELETE,
        "PATCH" => reqwest::Method::PATCH,
        "HEAD" => reqwest::Method::HEAD,
        "OPTIONS" => reqwest::Method::OPTIONS,
        other => match reqwest::Method::from_bytes(other.as_bytes()) {
            Ok(m) => m,
            Err(_) => {
                let _ = write_error_response(reader.get_mut(), 400, "Bad Request").await;
                return;
            }
        },
    };

    let mut builder = client.request(method, &req.uri);
    for (name, value) in &headers {
        builder = builder.header(name.as_str(), value.as_str());
    }
    if body_len > 0 {
        builder = builder.body(body);
    }

    let resp = match builder.send().await {
        Ok(r) => r,
        Err(e) => {
            warn!(peer = %peer, error = %e, "upstream error");
            let _ = write_error_response(reader.get_mut(), 502, "Bad Gateway").await;
            return;
        }
    };

    // Relay response back
    let status = resp.status().as_u16();
    let resp_headers: Vec<(String, String)> = resp
        .headers()
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_owned()))
        .collect();

    let mut clean_headers = resp_headers;
    strip_hop_by_hop(&mut clean_headers);

    let resp_body = match resp.bytes().await {
        Ok(b) => b,
        Err(e) => {
            warn!(peer = %peer, error = %e, "failed to read upstream body");
            let _ = write_error_response(reader.get_mut(), 502, "Bad Gateway").await;
            return;
        }
    };

    // Limit response body
    let body_slice = if resp_body.len() as u64 > MAX_RESPONSE_BODY {
        &resp_body[..MAX_RESPONSE_BODY as usize]
    } else {
        &resp_body
    };

    let stream = reader.get_mut();
    let status_line = format!("HTTP/1.1 {status} {}\r\n", reason_phrase(status));
    let _ = stream.write_all(status_line.as_bytes()).await;
    let _ = stream
        .write_all(format_headers(&clean_headers).as_bytes())
        .await;
    let _ = stream.write_all(b"\r\n").await;
    let _ = stream.write_all(body_slice).await;
    let _ = stream.flush().await;
}

/// Extract hostname from an absolute HTTP URI.
fn extract_host_from_uri(uri: &str) -> String {
    // Parse "http://host:port/path" → "host"
    if let Some(rest) = uri.strip_prefix("http://").or_else(|| uri.strip_prefix("https://")) {
        let host_part = rest.split('/').next().unwrap_or(rest);
        // Strip port
        if let Some(idx) = host_part.rfind(':') {
            return host_part[..idx].to_owned();
        }
        return host_part.to_owned();
    }
    // Might be just a path — check Host header at caller
    String::new()
}

/// Write an HTTP error response.
async fn write_error_response(
    stream: &mut TcpStream,
    status: u16,
    reason: &str,
) -> io::Result<()> {
    let response = format!(
        "HTTP/1.1 {status} {reason}\r\nContent-Length: 0\r\n\r\n"
    );
    stream.write_all(response.as_bytes()).await?;
    stream.flush().await
}

/// Simple status code → reason phrase mapping.
fn reason_phrase(status: u16) -> &'static str {
    match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        500 => "Internal Server Error",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        _ => "OK",
    }
}

// ── CONNECT handler ─────────────────────────────────────────────────

/// Handle an HTTP CONNECT request — dispatch to MITM or blind tunnel.
async fn handle_connect(
    reader: BufReader<TcpStream>,
    peer: SocketAddr,
    req: &RawRequest,
    state: &ProxyState,
) {
    let (host, host_port) = parse_connect_host(&req.uri);
    debug!(peer = %peer, host = %host, "CONNECT request");

    // Get the raw stream back from the reader
    let mut stream = reader.into_inner();

    // Send 200 Connection Established
    if let Err(e) = stream
        .write_all(b"HTTP/1.1 200 Connection Established\r\n\r\n")
        .await
    {
        debug!(peer = %peer, error = %e, "failed to send CONNECT response");
        return;
    }
    let _ = stream.flush().await;

    if state.host_allowed(&host) {
        tls_mitm(stream, peer, &host, &host_port, state).await;
    } else {
        tcp_tunnel(stream, peer, &host_port).await;
    }
}

// ── TLS MITM ────────────────────────────────────────────────────────

/// MITM TLS connection: terminate TLS with a generated cert, read plaintext
/// HTTP requests, replace credential headers, and forward to the real server.
async fn tls_mitm(
    stream: TcpStream,
    peer: SocketAddr,
    host: &str,
    host_port: &str,
    state: &ProxyState,
) {
    // Generate cert for this host
    let certified_key = match state.certs.get_or_create(host) {
        Ok(ck) => ck,
        Err(e) => {
            warn!(peer = %peer, host, error = %e, "cert generation error");
            return;
        }
    };

    // Build TLS server config using the CertifiedKey directly via cert resolver
    let resolver = CertResolver(certified_key);
    let mut tls_config = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_cert_resolver(Arc::new(resolver));
    tls_config.alpn_protocols = vec![b"http/1.1".to_vec()];
    let tls_config = Arc::new(tls_config);

    let acceptor = TlsAcceptor::from(tls_config);
    let tls_stream = match acceptor.accept(stream).await {
        Ok(s) => s,
        Err(e) => {
            warn!(peer = %peer, host, error = %e, "TLS handshake error");
            return;
        }
    };

    let host_port = ensure_port(host_port);
    info!(peer = %peer, host, "MITM TLS established");

    // Request loop — read HTTP requests from the TLS connection
    let mut reader = BufReader::new(tls_stream);
    loop {
        // Read next request with idle timeout
        let req = match timeout(CONN_IDLE_TIMEOUT, read_http_request(&mut reader)).await {
            Ok(Ok(req)) => req,
            Ok(Err(e)) => {
                if e.kind() == io::ErrorKind::UnexpectedEof {
                    // Clean close
                    return;
                }
                debug!(peer = %peer, host, error = %e, "MITM read error");
                return;
            }
            Err(_) => {
                // Idle timeout — clean close
                debug!(peer = %peer, host, "MITM idle timeout");
                return;
            }
        };

        // Fix up the request URL for forwarding
        let url = format!("https://{}{}", host_port, req.uri);

        // Read request body
        let body_len = content_length(&req.headers).min(MAX_REQUEST_BODY);
        let mut body = vec![0u8; body_len as usize];
        if body_len > 0 {
            if let Err(e) = reader.read_exact(&mut body).await {
                debug!(peer = %peer, error = %e, "failed to read MITM request body");
                return;
            }
        }

        let mut headers = req.headers.clone();
        replace_headers(&mut headers, host, state);
        strip_hop_by_hop(&mut headers);

        // Forward to upstream
        let client = reqwest::Client::builder()
            .redirect(reqwest::redirect::Policy::none())
            .build()
            .unwrap();

        let method = match req.method.to_ascii_uppercase().as_str() {
            "GET" => reqwest::Method::GET,
            "POST" => reqwest::Method::POST,
            "PUT" => reqwest::Method::PUT,
            "DELETE" => reqwest::Method::DELETE,
            "PATCH" => reqwest::Method::PATCH,
            "HEAD" => reqwest::Method::HEAD,
            "OPTIONS" => reqwest::Method::OPTIONS,
            other => match reqwest::Method::from_bytes(other.as_bytes()) {
                Ok(m) => m,
                Err(_) => return,
            },
        };

        let mut builder = client.request(method, &url);
        for (name, value) in &headers {
            builder = builder.header(name.as_str(), value.as_str());
        }
        if body_len > 0 {
            builder = builder.body(body);
        }

        // Set write timeout for response relay
        let resp = match timeout(CONN_READ_TIMEOUT, builder.send()).await {
            Ok(Ok(r)) => r,
            Ok(Err(e)) => {
                warn!(peer = %peer, host, error = %e, "upstream error");
                let _ = reader
                    .get_mut()
                    .write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n")
                    .await;
                return;
            }
            Err(_) => {
                warn!(peer = %peer, host, "upstream timeout");
                let _ = reader
                    .get_mut()
                    .write_all(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n")
                    .await;
                return;
            }
        };

        // Check Connection: close before consuming the response
        let conn_close = resp
            .headers()
            .get("connection")
            .and_then(|v| v.to_str().ok())
            .map(|v| v.eq_ignore_ascii_case("close"))
            .unwrap_or(false);
        let req_close = find_header(&req.headers, "connection")
            .map(|v| v.eq_ignore_ascii_case("close"))
            .unwrap_or(false);

        let status = resp.status().as_u16();
        let resp_headers: Vec<(String, String)> = resp
            .headers()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_owned()))
            .collect();

        let mut clean_headers = resp_headers;
        strip_hop_by_hop(&mut clean_headers);

        let resp_body = match resp.bytes().await {
            Ok(b) => b,
            Err(e) => {
                warn!(peer = %peer, host, error = %e, "failed to read upstream body");
                return;
            }
        };

        // Limit response body
        let body_slice = if resp_body.len() as u64 > MAX_RESPONSE_BODY {
            &resp_body[..MAX_RESPONSE_BODY as usize]
        } else {
            &resp_body
        };

        // Write response back through TLS
        let tls = reader.get_mut();
        let status_line = format!("HTTP/1.1 {status} {}\r\n", reason_phrase(status));
        if tls.write_all(status_line.as_bytes()).await.is_err() {
            return;
        }

        // Add Content-Length if not already present
        let has_content_length = clean_headers
            .iter()
            .any(|(n, _)| n.eq_ignore_ascii_case("content-length"));
        if !has_content_length {
            let cl = format!("Content-Length: {}\r\n", body_slice.len());
            if tls.write_all(cl.as_bytes()).await.is_err() {
                return;
            }
        }

        if tls
            .write_all(format_headers(&clean_headers).as_bytes())
            .await
            .is_err()
        {
            return;
        }
        if tls.write_all(b"\r\n").await.is_err() {
            return;
        }
        if tls.write_all(body_slice).await.is_err() {
            return;
        }
        if tls.flush().await.is_err() {
            return;
        }

        // Check if either side requested close
        if conn_close || req_close {
            return;
        }
    }
}

// ── Blind TCP tunnel ────────────────────────────────────────────────

/// Blind bidirectional TCP tunnel — no TLS termination, no inspection.
async fn tcp_tunnel(client_stream: TcpStream, peer: SocketAddr, host_port: &str) {
    let host_port = ensure_port(host_port);
    debug!(peer = %peer, target = %host_port, "TCP tunnel");

    // Connect to upstream with 10s timeout (matches Go)
    let upstream = match timeout(Duration::from_secs(10), TcpStream::connect(&host_port)).await {
        Ok(Ok(s)) => s,
        Ok(Err(e)) => {
            warn!(peer = %peer, target = %host_port, error = %e, "tunnel dial error");
            return;
        }
        Err(_) => {
            warn!(peer = %peer, target = %host_port, "tunnel dial timeout");
            return;
        }
    };

    // Split both streams for bidirectional copy
    let (mut client_read, mut client_write) = tokio::io::split(client_stream);
    let (mut upstream_read, mut upstream_write) = tokio::io::split(upstream);

    // Set absolute deadline via tokio::time::timeout on both copy directions
    let tunnel_dur = TUNNEL_TIMEOUT;

    let client_to_upstream = async {
        let _ = tokio::io::copy(&mut client_read, &mut upstream_write).await;
    };

    let upstream_to_client = async {
        let _ = tokio::io::copy(&mut upstream_read, &mut client_write).await;
    };

    // Run both directions with overall tunnel timeout
    // When one direction finishes, we're done (like the Go version)
    let _ = timeout(tunnel_dur, async {
        tokio::select! {
            _ = client_to_upstream => {},
            _ = upstream_to_client => {},
        }
    })
    .await;

    debug!(peer = %peer, target = %host_port, "tunnel closed");
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_connect_host_with_port() {
        let (host, host_port) = parse_connect_host("example.com:443");
        assert_eq!(host, "example.com");
        assert_eq!(host_port, "example.com:443");
    }

    #[test]
    fn parse_connect_host_without_port() {
        let (host, host_port) = parse_connect_host("example.com");
        assert_eq!(host, "example.com");
        assert_eq!(host_port, "example.com");
    }

    #[test]
    fn ensure_port_adds_443() {
        assert_eq!(ensure_port("example.com"), "example.com:443");
        assert_eq!(ensure_port("example.com:443"), "example.com:443");
        assert_eq!(ensure_port("example.com:8080"), "example.com:8080");
    }

    #[test]
    fn extract_host_from_http_uri() {
        assert_eq!(
            extract_host_from_uri("http://example.com/path"),
            "example.com"
        );
        assert_eq!(
            extract_host_from_uri("http://example.com:8080/path"),
            "example.com"
        );
        assert_eq!(
            extract_host_from_uri("https://api.example.com/v1"),
            "api.example.com"
        );
    }

    #[test]
    fn strip_hop_by_hop_removes_headers() {
        let mut headers = vec![
            ("Connection".to_owned(), "keep-alive".to_owned()),
            ("Content-Type".to_owned(), "text/plain".to_owned()),
            ("Keep-Alive".to_owned(), "timeout=5".to_owned()),
            ("Authorization".to_owned(), "Bearer token".to_owned()),
            ("Proxy-Authorization".to_owned(), "Basic xxx".to_owned()),
            ("Transfer-Encoding".to_owned(), "chunked".to_owned()),
        ];
        strip_hop_by_hop(&mut headers);
        assert_eq!(headers.len(), 2);
        assert_eq!(headers[0].0, "Content-Type");
        assert_eq!(headers[1].0, "Authorization");
    }

    #[test]
    fn content_length_parsing() {
        let headers = vec![("Content-Length".to_owned(), "42".to_owned())];
        assert_eq!(content_length(&headers), 42);

        let headers = vec![("X-Something".to_owned(), "foo".to_owned())];
        assert_eq!(content_length(&headers), 0);
    }

    #[tokio::test]
    async fn read_http_request_parses_get() {
        let raw = b"GET /path HTTP/1.1\r\nHost: example.com\r\nAccept: */*\r\n\r\n";
        let mut cursor = &raw[..];
        let mut reader = BufReader::new(&mut cursor);
        let req = read_http_request(&mut reader).await.unwrap();
        assert_eq!(req.method, "GET");
        assert_eq!(req.uri, "/path");
        assert_eq!(req.version, "HTTP/1.1");
        assert_eq!(req.headers.len(), 2);
        assert_eq!(req.headers[0].0, "Host");
        assert_eq!(req.headers[0].1, "example.com");
    }

    #[tokio::test]
    async fn read_http_request_parses_connect() {
        let raw = b"CONNECT api.example.com:443 HTTP/1.1\r\nHost: api.example.com:443\r\n\r\n";
        let mut cursor = &raw[..];
        let mut reader = BufReader::new(&mut cursor);
        let req = read_http_request(&mut reader).await.unwrap();
        assert_eq!(req.method, "CONNECT");
        assert_eq!(req.uri, "api.example.com:443");
        assert_eq!(req.version, "HTTP/1.1");
    }

    #[test]
    fn reason_phrase_returns_correct_values() {
        assert_eq!(reason_phrase(200), "OK");
        assert_eq!(reason_phrase(404), "Not Found");
        assert_eq!(reason_phrase(502), "Bad Gateway");
    }
}
