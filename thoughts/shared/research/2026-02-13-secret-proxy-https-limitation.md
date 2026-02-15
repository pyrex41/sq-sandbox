---
date: 2026-02-13T16:04:50-06:00
researcher: reuben
git_commit: bc03a75
branch: main
repository: sq-sandbox
topic: "Why can't the secret proxy handle HTTPS?"
tags: [research, codebase, secret-proxy, https, tls, security]
status: complete
last_updated: 2026-02-13
last_updated_by: reuben
---

# Research: Why can't the secret proxy handle HTTPS?

**Date**: 2026-02-13T16:04:50-06:00
**Researcher**: reuben
**Git Commit**: bc03a75
**Branch**: main
**Repository**: sq-sandbox

## Research Question
Why can't we use the secret proxy for HTTPS also? Seems kind of pointless since everything's HTTPS.

## Summary

The secret proxy (`bin/sq-secret-proxy`) works by reading **plaintext HTTP request headers**, scanning them for placeholder strings (e.g., `sk-placeholder-anthropic`), and replacing them with real credentials before forwarding the request. This fundamentally requires seeing the request headers in the clear.

HTTPS makes this impossible because the client encrypts the entire request (headers, body, everything) end-to-end with the destination server. The proxy only sees an opaque `CONNECT` tunnel request — it never sees the actual headers containing the placeholders.

## How the proxy works today

### 1. Sandbox gets placeholders and proxy config (`cgi-bin/common.sh:253-279`)

When a sandbox is created, `_inject_secret_placeholders()` writes a shell profile script at `$sandbox/upper/data/etc/profile.d/squash-secrets.sh` containing:

```sh
export ANTHROPIC_API_KEY=sk-placeholder-anthropic   # fake value
export http_proxy=http://10.200.1.1:8888            # routes through proxy
export https_proxy=http://10.200.1.1:8888           # also set, but...
```

### 2. Sandbox makes an HTTP request

When sandbox code does `curl http://api.anthropic.com/v1/messages`, curl respects `http_proxy` and sends the full HTTP request to the proxy instead of the destination:

```
GET http://api.anthropic.com/v1/messages HTTP/1.1
Host: api.anthropic.com
Authorization: Bearer sk-placeholder-anthropic    <-- placeholder in the clear
Content-Type: application/json
```

### 3. Proxy reads, replaces, forwards (`bin/sq-secret-proxy:22-126`)

The proxy (`socat` + shell handler):
1. Reads the raw HTTP request line-by-line (line 27-28)
2. Extracts the destination host from the URL (line 36)
3. Scans each header for placeholder strings (lines 53-70)
4. If the destination host is in `allowed_hosts`, replaces the placeholder with the real credential (line 63)
5. Forwards the modified request via `curl -K` (lines 82-113)

The real credential (`sk-ant-real-key-here`) is injected only on the wire between the proxy (running on the host) and the destination server. It never enters the sandbox.

## Why HTTPS breaks this

### What happens when a client uses HTTPS through an HTTP proxy

When sandbox code does `curl https://api.anthropic.com/v1/messages`, curl sees `https_proxy` is set and sends a **CONNECT** request to the proxy:

```
CONNECT api.anthropic.com:443 HTTP/1.1
Host: api.anthropic.com:443
```

This asks the proxy to open a raw TCP tunnel to `api.anthropic.com:443`. Once the proxy connects, the client performs TLS directly with the destination server **through** the tunnel. From the proxy's perspective, everything after the CONNECT handshake is opaque encrypted bytes.

The actual HTTP request with the `Authorization: Bearer sk-placeholder-anthropic` header is inside the TLS session. The proxy never sees it.

### Why "just add CONNECT support" doesn't help

Even if the shell proxy handled `CONNECT` (today it doesn't — it only parses plain HTTP), it would just be a dumb TCP pipe. The placeholder replacement happens by string-matching on header lines, which are encrypted.

### The only way to make it work: TLS MITM

To inspect and modify HTTPS requests, the proxy would need to:

1. **Terminate TLS** — accept the client's TLS connection using a proxy-generated certificate
2. **Read the plaintext** — scan headers for placeholders, replace them
3. **Re-encrypt** — open a new TLS connection to the destination and forward the modified request

This requires:
- A certificate authority (CA) trusted by the sandbox
- Per-destination certificate generation (or a wildcard interception cert)
- A compiled binary that handles TLS (shell + socat can't do this)
- The CA cert injected into the sandbox's trust store

This is a fundamentally different architecture — it's a MITM proxy like mitmproxy, Fiddler, or Burp Suite. It's roughly 200 lines of Go or Rust (noted in the proxy source at line 7), but it's not a simple extension of the current shell script.

### Why `https_proxy` is still set

The injection code sets `https_proxy` anyway (`cgi-bin/common.sh:274-276`). This is because some HTTP client libraries will attempt to use the proxy for HTTPS via CONNECT. Since the proxy doesn't handle CONNECT, these requests will simply fail — the placeholder is never sent to the destination, so no credential leaks. It's a fail-closed behavior.

## Current architecture flow

```
Sandbox (sees placeholders)
  │
  ├── HTTP request ──→ sq-secret-proxy:8888 ──→ destination
  │                    (reads headers,            (gets real
  │                     replaces placeholders)     credentials)
  │
  └── HTTPS request ──→ sq-secret-proxy:8888 ──→ FAILS
                        (gets CONNECT request,
                         can't see encrypted headers)
```

## Practical impact

Most modern APIs (Anthropic, OpenAI, GitHub, etc.) enforce HTTPS. This means the secret proxy's placeholder replacement effectively only works for:
- HTTP-only APIs (rare)
- Environments where TLS is terminated before reaching the sandbox (API gateway, reverse proxy on the host)
- The Firecracker backend where networking is handled differently

## Code References
- `bin/sq-secret-proxy:1-132` - Full proxy implementation (socat + shell handler)
- `bin/sq-secret-proxy:34-36` - Host extraction from URL (only works for plain HTTP URLs)
- `bin/sq-secret-proxy:52-70` - Placeholder scanning and replacement in headers
- `cgi-bin/common.sh:253-279` - `_inject_secret_placeholders()` injection into sandbox
- `cgi-bin/common.sh:273-276` - Sets both `http_proxy` and `https_proxy`

## Historical Context (from thoughts/)
- `thoughts/shared/plans/plan-firecracker-and-security.md` - Original design notes: "A production-grade proxy for HTTPS will require a compiled binary — a ~200-line Go or Rust program that does CONNECT tunneling with header rewriting"
- `thoughts/shared/research/2026-02-13-security-model-comparison.md` - Compares to Deno Sandbox which also uses HTTP proxy approach
- `thoughts/shared/research/2026-02-13-security-hardening-pentest.md` - Identifies HTTPS bypass as attack surface
