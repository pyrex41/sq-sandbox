#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

UBUNTU_IMAGE="${UBUNTU_IMAGE:-ubuntu:24.04}"
ODIN_TAG="${ODIN_TAG:-dev-2026-02}"

if [[ -n "${ODIN_ARCH:-}" ]]; then
  arch="${ODIN_ARCH}"
else
  case "$(uname -m)" in
    arm64|aarch64) arch="arm64" ;;
    x86_64|amd64) arch="amd64" ;;
    *)
      echo "Unsupported host architecture: $(uname -m)"
      echo "Set ODIN_ARCH=amd64 or ODIN_ARCH=arm64 and retry."
      exit 1
      ;;
  esac
fi

odin_tarball="odin-linux-${arch}-${ODIN_TAG}.tar.gz"
odin_url="https://github.com/odin-lang/Odin/releases/download/${ODIN_TAG}/${odin_tarball}"

echo "Running Odin Linux validation in Docker"
echo "  image: ${UBUNTU_IMAGE}"
echo "  odin:  ${ODIN_TAG} (${arch})"
echo "  repo:  ${REPO_ROOT}"

docker run --rm \
  -v "${REPO_ROOT}:/work" \
  -w /work \
  "${UBUNTU_IMAGE}" \
  bash -lc "set -euo pipefail; \
    apt-get update >/dev/null; \
    DEBIAN_FRONTEND=noninteractive apt-get install -y curl ca-certificates clang lld build-essential >/dev/null; \
    cd /tmp; \
    curl -fsSL -o odin.tar.gz '${odin_url}'; \
    mkdir -p odin; \
    tar -xzf odin.tar.gz -C odin --strip-components=1; \
    /tmp/odin/odin version; \
    cd /work; \
    /tmp/odin/odin check src; \
    /tmp/odin/odin build src; \
    /tmp/odin/odin test test/exec/integration_test.odin -file"

