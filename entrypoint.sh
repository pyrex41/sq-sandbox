#!/bin/sh
set -eu
BIN="$(cd "$(dirname "$0")/bin" && pwd)"
export PATH="$BIN:$PATH"

echo "squash v3"

# Secret proxy — HTTPS mode (Go MITM) or HTTP mode (shell)
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
        sq-secret-proxy-https &
    else
        sq-secret-proxy &
    fi
fi

[ -n "${TAILSCALE_AUTHKEY:-}" ] && setup-tailscale &
sq-init
sq-reaper &
exec start-api
