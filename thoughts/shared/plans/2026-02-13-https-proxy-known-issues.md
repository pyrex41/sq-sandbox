# HTTPS Proxy: Known Issue Remediation Plan

Three issues found during local testing that need fixes before broader deployment.

---

## Issue 1: DNS resolution broken inside sandbox netns

**Symptom**: `wget: bad address 'api.x.ai'` from inside sandbox.

**Root cause**: `_chroot_create_sandbox()` writes `nameserver 10.200.x.1` into
the sandbox's `/etc/resolv.conf` (line 495 of `common.sh`). The gateway IP
`10.200.x.1` is the host side of the veth pair. But nothing listens on port 53
at that address — the host container's own DNS resolver is at a different IP
(e.g., `0.250.250.200` on OrbStack, `127.0.0.11` on Docker).

DNS packets from the sandbox hit the gateway, get MASQUERADE'd, and route to the
host's default route — but since the destination is the gateway itself (not a
routable address), the packets never reach an actual DNS server.

**Fix**: Add a DNAT rule in `_chroot_setup_netns()` to redirect DNS queries
arriving at the gateway to the host's actual resolver.

### File: `cgi-bin/common.sh`

In `_chroot_setup_netns()`, after the MASQUERADE rule (line 359), add:

```sh
    # DNS forwarding — redirect queries to the gateway to the host's real resolver
    local host_dns
    host_dns=$(awk '/^nameserver/{print $2; exit}' /etc/resolv.conf 2>/dev/null || echo "")
    if [ -n "$host_dns" ]; then
        iptables -t nat -A PREROUTING -s "10.200.${sandbox_index}.0/30" \
            -d "10.200.${sandbox_index}.1" -p udp --dport 53 \
            -j DNAT --to-destination "$host_dns"
        iptables -t nat -A PREROUTING -s "10.200.${sandbox_index}.0/30" \
            -d "10.200.${sandbox_index}.1" -p tcp --dport 53 \
            -j DNAT --to-destination "$host_dns"
    fi
```

In `_chroot_teardown_netns()`, add cleanup (after the existing MASQUERADE delete):

```sh
    if [ -n "$sandbox_index" ]; then
        local host_dns
        host_dns=$(awk '/^nameserver/{print $2; exit}' /etc/resolv.conf 2>/dev/null || echo "")
        if [ -n "$host_dns" ]; then
            iptables -t nat -D PREROUTING -s "10.200.${sandbox_index}.0/30" \
                -d "10.200.${sandbox_index}.1" -p udp --dport 53 \
                -j DNAT --to-destination "$host_dns" 2>/dev/null || true
            iptables -t nat -D PREROUTING -s "10.200.${sandbox_index}.0/30" \
                -d "10.200.${sandbox_index}.1" -p tcp --dport 53 \
                -j DNAT --to-destination "$host_dns" 2>/dev/null || true
        fi
    fi
```

**Why DNAT and not changing resolv.conf**: The gateway IP is the correct
nameserver from the sandbox's perspective — it's routable and on the same /30
subnet. DNAT transparently redirects to wherever the host actually resolves DNS.
This also works if the host's resolver changes (container restart, DHCP renewal).

**Scope**: Pre-existing bug, affects all sandbox networking, not just the HTTPS
proxy. Fixing this unblocks the proxy test path and fixes DNS for all sandboxes.

---

## Issue 2: Python `requests` ignores system CA bundle

**Symptom**: Python code using `import requests; requests.get("https://api.x.ai/...")`
would get `SSLCertVerificationError` even though the proxy CA is in the system
bundle, because `requests` uses its own `certifi` bundle.

**Root cause**: Python's `requests` library bundles its own CA certs via the
`certifi` package and does not read `/etc/ssl/certs/ca-certificates.crt` by
default. However, it respects two env vars:
- `REQUESTS_CA_BUNDLE` — used by `requests` specifically
- `SSL_CERT_FILE` — used by Python's `ssl` module (covers `urllib3`, `httpx`, etc.)

**Fix**: Add these env vars to the CA injection block in `_inject_secret_placeholders()`.

### File: `cgi-bin/common.sh`

In the `if [ -f "$ca_cert" ]` block (line 282), after the `NODE_EXTRA_CA_CERTS`
line, add:

```sh
        # Python: requests uses certifi, not system bundle; ssl module checks SSL_CERT_FILE
        echo "export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt" \
            >> "$env_dir/squash-secrets.sh"
        echo "export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt" \
            >> "$env_dir/squash-secrets.sh"
```

These point at the system bundle (which already has our CA appended). This covers:
- `requests.get()` — via `REQUESTS_CA_BUNDLE`
- `urllib3` — via `SSL_CERT_FILE`
- `httpx` — via `SSL_CERT_FILE`
- `aiohttp` — via `SSL_CERT_FILE`
- Any Python code using `ssl.create_default_context()` — via `SSL_CERT_FILE`

**No harm when HTTPS proxy is off**: The block only runs when the CA file exists
(i.e., `SQUASH_PROXY_HTTPS=1`). The env vars point to the real system bundle
which is valid regardless.

### File: `bin/sq-test`

Add a test for the new env vars in the Secret Proxy section (after the
`NODE_EXTRA_CA_CERTS` check):

```sh
    _log "SSL_CERT_FILE set in env"
    grep -q "SSL_CERT_FILE" \
        "$SANDBOXES/test-sec/upper/data/etc/profile.d/squash-secrets.sh" 2>/dev/null \
        && _pass || _skip "no SSL_CERT_FILE (SQUASH_PROXY_HTTPS not enabled)"

    _log "REQUESTS_CA_BUNDLE set in env"
    grep -q "REQUESTS_CA_BUNDLE" \
        "$SANDBOXES/test-sec/upper/data/etc/profile.d/squash-secrets.sh" 2>/dev/null \
        && _pass || _skip "no REQUESTS_CA_BUNDLE (SQUASH_PROXY_HTTPS not enabled)"
```

And add two more `_skip "no secrets.json"` lines to the else branch.

---

## Issue 3: Stale base module missing `ca-certificates`

**Symptom**: The system CA bundle (`/etc/ssl/certs/ca-certificates.crt`) doesn't
exist in sandboxes built from base modules created before the `ca-certificates`
package was added to `sq-mkbase`. The CA injection block silently skips
appending to the bundle (the `if [ -f "$s/merged/etc/ssl/certs/..." ]` check
fails), so HTTPS clients in the sandbox can't verify any TLS cert at all.

**Root cause**: `sq-mkbase` now includes `ca-certificates` in the Alpine package
list, but `sq-init` only builds the base if it doesn't already exist (line 15
of `sq-init`). Persistent volumes from before the update still have the old
base module.

**Fix**: Version-stamp the base module so `sq-init` can detect stale modules.

### File: `bin/sq-mkbase`

At the end of `build_alpine()`, after `squash_it`, write a version marker:

```sh
    echo "2" > "$MODULES/000-base-alpine.version"
```

### File: `bin/sq-init`

Change the base module check from a simple existence check to a version check:

```sh
# Current:
if [ ! -f "$MODULES/000-base-alpine.squashfs" ]; then

# New:
_base_current=2
_base_version=$(cat "$MODULES/000-base-alpine.version" 2>/dev/null || echo "0")
if [ ! -f "$MODULES/000-base-alpine.squashfs" ] || [ "$_base_version" -lt "$_base_current" ]; then
```

This triggers a rebuild on the next container start if the base is outdated.
The S3 pull path should also check the version (pull won't help if S3 has the
old version too — it'll fall through to local build).

**Alternative (simpler, less robust)**: Just bump the base filename to
`000-base-alpine-v2.squashfs`. Rejected because it breaks all existing
`layers` references and sandbox manifests.

**Alternative (even simpler)**: Document "run `sq-mkbase alpine` to rebuild"
after upgrading. Acceptable for now but fragile for automated deployments.

The version stamp approach is the right middle ground — automated, backward
compatible, and cheap.

---

## File summary

| File | Change |
|------|--------|
| `cgi-bin/common.sh` | DNS DNAT rules in `_chroot_setup_netns()` / `_chroot_teardown_netns()`, Python env vars in `_inject_secret_placeholders()` |
| `bin/sq-init` | Version check for stale base module |
| `bin/sq-mkbase` | Write version marker after base build |
| `bin/sq-test` | Add `SSL_CERT_FILE` and `REQUESTS_CA_BUNDLE` test assertions |

## Verification

1. Build + run with `SQUASH_PROXY_HTTPS=1` and a test `secrets.json`
2. Create sandbox → verify `/etc/resolv.conf` still shows `10.200.x.1`
3. From sandbox: `nslookup api.x.ai` → should resolve (DNS DNAT working)
4. From sandbox: `wget https://api.x.ai/v1/models` with auth header → should succeed
5. Check `squash-secrets.sh` contains `SSL_CERT_FILE` and `REQUESTS_CA_BUNDLE`
6. Delete `000-base-alpine.version`, restart container → should rebuild base
