#!/bin/sh
# test/run.sh — build container and run integration tests
# Usage: test/run.sh [--keep]
# --keep: don't remove the container after tests (for debugging)
set -eu

cd "$(dirname "$0")/.."

IMAGE="sq-sandbox:test"
CONTAINER="sq-test-$$"
KEEP=false
[ "${1:-}" = "--keep" ] && KEEP=true

echo "=== Building test image ==="
docker build -t "$IMAGE" .

echo "=== Starting container ==="
docker run -d --name "$CONTAINER" --privileged \
    -e SQUASH_AUTH_TOKEN="" \
    "$IMAGE"

echo "=== Waiting for API ==="
retries=0
while ! docker exec "$CONTAINER" curl -sf http://localhost:8080/cgi-bin/health >/dev/null 2>&1; do
    retries=$((retries + 1))
    [ $retries -gt 60 ] && { echo "TIMEOUT"; docker logs "$CONTAINER"; exit 1; }
    sleep 1
done

echo "=== Injecting test secrets ==="
docker cp test/secrets.json "$CONTAINER":/data/secrets.json

echo "=== Running tests ==="
rc=0
docker exec "$CONTAINER" /app/bin/sq-test || rc=$?

if [ "$KEEP" = "false" ]; then
    echo "=== Cleaning up ==="
    docker rm -f "$CONTAINER" >/dev/null
fi

exit $rc
