#!/bin/sh
set -eu
BIN="$(cd "$(dirname "$0")/bin" && pwd)"
export PATH="$BIN:$PATH"

echo "squash v3"

[ -n "${TAILSCALE_AUTHKEY:-}" ] && setup-tailscale &
sq-init
sq-reaper &
[ -f "${SQUASH_DATA:-/data}/secrets.json" ] && sq-secret-proxy &
exec start-api
