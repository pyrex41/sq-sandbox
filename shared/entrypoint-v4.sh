#!/bin/sh
set -eu
BIN="$(cd "$(dirname "$0")/bin" && pwd)"
export PATH="$BIN:$PATH"

echo "squash v4"

# Generate proxy CA certificate if HTTPS proxy enabled and secrets exist.
# Must happen before the daemon starts — the Go proxy reads certs at init.
if [ -f "${SQUASH_DATA:-/data}/secrets.json" ]; then
    if [ "${SQUASH_PROXY_HTTPS:-}" = "1" ]; then
        _proxy_ca_dir="${SQUASH_DATA:-/data}/proxy-ca"
        if [ ! -f "$_proxy_ca_dir/ca.crt" ]; then
            mkdir -p "$_proxy_ca_dir"
            openssl req -new -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
                -days 3650 -nodes -x509 \
                -subj "/CN=sq-secret-proxy CA" \
                -keyout "$_proxy_ca_dir/ca.key" \
                -out "$_proxy_ca_dir/ca.crt" 2>/dev/null
            chmod 600 "$_proxy_ca_dir/ca.key"
            echo "[entrypoint] generated proxy CA certificate" >&2
        fi
    else
        # Shell-based HTTP-only secret proxy (no MITM, no TLS)
        sq-secret-proxy &
    fi
fi

# Tailscale network setup (background — non-blocking)
[ -n "${TAILSCALE_AUTHKEY:-}" ] && setup-tailscale &

# The SBCL daemon handles everything:
#   - Init/recovery (replaces sq-init)
#   - Reaper thread (replaces sq-reaper)
#   - HTTP API server (replaces busybox httpd + CGI)
#   - Go proxy launch (replaces entrypoint proxy start)
exec squashd
