#!/bin/sh
set -eu
BIN="$(cd "$(dirname "$0")/bin" && pwd)"
export PATH="$BIN:$PATH"

echo "squash v3"

[ -n "${TAILSCALE_AUTHKEY:-}" ] && setup-tailscale &
sq-init
exec start-api
