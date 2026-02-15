# HTTPS Proxy Test Matrix

Status: **not started** — only tested locally on Docker Desktop (OrbStack, arm64 macOS).

## What we're testing

The Go MITM proxy (`SQUASH_PROXY_HTTPS=1`) generates a CA at startup, issues
per-host TLS certs, intercepts HTTPS CONNECT tunnels to allowed_hosts, and
injects credentials. This touches kernel features (network namespaces, veth,
iptables NAT), TLS libraries, and trust store paths — all of which vary across
environments.

---

## Test axes

### 1. Architecture (binary works on both)

- [ ] **amd64** — most cloud VMs, CI runners
- [ ] **arm64** — Graviton, Apple Silicon, Ampere

The Go binary is `CGO_ENABLED=0` static, so this should just work via
multi-platform Docker build. But TLS handshake behavior can differ.

### 2. Container runtime

- [ ] **Docker (default)** — tested locally on OrbStack/macOS
- [ ] **Podman (rootless)** — `--privileged` behaves differently; veth creation
      may fail; confirm proxy still starts and MITM works even if netns fails
- [ ] **containerd / nerdctl** — ECS uses containerd; confirm no runtime-specific
      socket or cgroup issues

### 3. Cloud / hosting environment

- [ ] **AWS ECS Fargate** — no `--privileged`, but `SYS_ADMIN` + `NET_ADMIN` caps
      may suffice for our use case. Key question: can we create network namespaces?
      If not, sandboxes fall back to localhost proxy (10.200.x.1 → 127.0.0.1).
      Proxy itself should work regardless.
- [ ] **AWS ECS on EC2** — full privileged support. Test with Graviton (arm64) and
      Intel/AMD (amd64) instance types.
- [ ] **Fly.io** — uses Firecracker under the hood. Privileged containers require
      `[build] privileged = true` in fly.toml. Confirm veth + iptables work.
- [ ] **Bare metal / VPS** — any Linux host with Docker. Test on a cheap VPS
      (Hetzner, DigitalOcean) to confirm no provider-specific networking quirks.

### 4. Sandbox backend

- [ ] **chroot** (default) — full test: netns, veth, iptables, overlayfs, CA injection.
      This is the primary path.
- [ ] **firecracker** — proxy runs on the host side. Guest VM connects via vsock or
      tap network. CA injection path is different (currently not implemented for
      firecracker). Verify proxy starts and doesn't crash; note that secret
      injection for firecracker is a separate TODO.

### 5. Sandbox base distro (trust store paths)

The CA gets injected into `/etc/ssl/certs/ca-certificates.crt` (Debian/Alpine
convention) and `/usr/local/share/ca-certificates/`. Different distros use
different paths and tools.

- [ ] **Alpine** (tested locally) — uses `ca-certificates` package,
      `/etc/ssl/certs/ca-certificates.crt` bundle
- [ ] **Debian/Ubuntu** — same bundle path but `update-ca-certificates` expects
      certs in `/usr/local/share/ca-certificates/` (which we write to).
      Verify `curl`, `python3 requests`, `wget` all trust the CA.
- [ ] **Void Linux** — uses `/etc/ssl/certs/ca-certificates.crt` (same as Alpine).
      Should work but untested.

### 6. Client runtimes inside sandbox

- [ ] **curl** — reads system CA bundle. Should work with our injection.
- [ ] **wget** — reads system CA bundle. Tested locally, works.
- [ ] **Python `requests`** — uses `certifi` by default, NOT the system bundle.
      May need `REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt` or
      `SSL_CERT_FILE` env var. **Likely needs a fix.**
- [ ] **Node.js** — `NODE_EXTRA_CA_CERTS` is set. Verify it works with `fetch`,
      `axios`, `node-fetch`.
- [ ] **Go programs** — reads system bundle on Linux. Should work.
- [ ] **Ruby** — uses OpenSSL system store. Should work.
- [ ] **Java** — uses its own truststore (`cacerts`). Would need `keytool` import.
      **Out of scope** for now but worth noting.

### 7. Network configurations

- [ ] **With `allow_net`** — iptables egress rules + proxy. Verify allowed hosts
      can be reached via HTTPS through the proxy, blocked hosts are dropped.
- [ ] **Without `allow_net`** — all egress open. Proxy still intercepts allowed_hosts,
      tunnels everything else.
- [ ] **Behind corporate proxy / NAT** — the container itself might be behind a
      proxy. Our proxy chains to `DefaultTransport` which respects `HTTP_PROXY`
      on the host side. Could cause double-proxy issues. Need to verify or
      explicitly clear host proxy env in the Go binary.
- [ ] **IPv6-only host** — veth addressing is IPv4 (10.200.x.x). Proxy binds on
      `:8888` (all interfaces). Should work on dual-stack but untested on
      IPv6-only.

### 8. Feature flag off (regression)

- [ ] **`SQUASH_PROXY_HTTPS` unset** — shell proxy runs, no CA generated, no CA
      injected into sandboxes. Existing tests pass. Zero behavior change.
- [ ] **`SQUASH_PROXY_HTTPS` unset, secrets.json present** — same as above.
      Verify no proxy-ca directory created.
- [ ] **No secrets.json** — neither proxy starts. No errors.

---

## Priority order

1. **ECS on EC2 (amd64 + arm64)** — primary deployment target
2. **Feature flag off regression** — must not break existing deployments
3. **Python `requests` trust store** — likely needs `SSL_CERT_FILE` env var fix
4. **Fly.io** — secondary deployment target
5. **Podman rootless** — community use case
6. **Debian/Ubuntu base** — second most common base distro
7. Everything else

## Known issues found during local testing

- DNS resolution fails inside sandbox netns (resolv.conf points to gateway but
  no DNS forwarder runs there). Pre-existing, not proxy-specific.
- veth creation may silently fail on some runtimes. Proxy still works if sandbox
  can reach 127.0.0.1:8888 directly.
- Alpine base module built at container startup may not include `ca-certificates`
  package until the base is rebuilt with the updated `sq-mkbase`.
