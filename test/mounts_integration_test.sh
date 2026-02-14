#!/usr/bin/env bash
#
# Integration test runner for mount cycle tests.
# Runs tests in a privileged Docker container with necessary capabilities.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Build test container image
echo "Building test container..."
docker build -t sq-mounts-test -f - "$PROJECT_ROOT" <<'EOF'
FROM rust:1.75-alpine

# Install required tools
RUN apk add --no-cache \
    musl-dev \
    squashfs-tools \
    util-linux \
    grep

WORKDIR /workspace

# Copy project files
COPY . .

# Build with test profile
RUN cargo build --bin squashd --tests

CMD ["cargo", "test", "--bin", "squashd", "mounts", "--", "--test-threads=1", "--nocapture"]
EOF

echo ""
echo "Running mount integration tests in privileged container..."
echo "==============================================================="
echo ""

# Run tests with required capabilities:
# - SYS_ADMIN: for mount/unmount operations
# - MKNOD: for creating device nodes (if needed)
docker run --rm \
    --cap-add=SYS_ADMIN \
    --cap-add=MKNOD \
    --security-opt apparmor=unconfined \
    sq-mounts-test

echo ""
echo "==============================================================="
echo "Tests completed successfully!"
