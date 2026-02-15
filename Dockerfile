# Squash v4 — Common Lisp rewrite
# Multi-stage build: SBCL daemon + Go proxy + Alpine runtime
# Requires: --privileged (for mount, overlayfs, loop, unshare)

# ── Stage 1: Build SBCL daemon image ─────────────────────────────────
# Ubuntu has the best SBCL packaging; Quicklisp pulls all CL dependencies.
# save-lisp-and-die produces a standalone executable (~20MB compressed).
FROM ubuntu:24.04 AS sbcl-build

RUN apt-get update && apt-get install -y --no-install-recommends \
    sbcl \
    curl \
    ca-certificates \
    build-essential \
    libffi-dev \
    libev-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Quicklisp
RUN curl -sO https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --non-interactive --load quicklisp.lisp \
            --eval '(quicklisp-quickstart:install :path "/root/quicklisp")' \
            --quit \
    && rm quicklisp.lisp \
    && echo '#-quicklisp (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' > /root/.sbclrc

# Pre-fetch dependencies (cached layer — only busted when .asd changes)
COPY squashd.asd /build/
WORKDIR /build
RUN sbcl --non-interactive \
         --eval '(push #P"/build/" asdf:*central-registry*)' \
         --eval '(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart (find-restart (quote continue))))))' \
         --eval '(ql:quickload "squashd" :silent t)' \
         --quit 2>/dev/null; true

# Copy source and build the image
COPY src/ /build/src/
RUN sbcl --non-interactive \
         --eval '(push #P"/build/" asdf:*central-registry*)' \
         --eval '(ql:quickload "squashd")' \
         --eval '(squashd::build-image :output "/build/squashd")' \
    && ls -lh /build/squashd

# Collect glibc runtime libs needed by the SBCL binary.
# We copy the actual files (resolving symlinks) plus the dynamic linker.
RUN mkdir -p /sbcl-libs && \
    # Copy resolved shared libs
    for lib in $(ldd /build/squashd | awk '/=>/ {print $3}' | grep -v 'not found'); do \
        cp -L "$lib" /sbcl-libs/; \
    done && \
    # Copy the dynamic linker (resolve the actual interpreter path from ELF header)
    interp=$(readelf -l /build/squashd | awk '/interpreter:/ {gsub(/[\[\]]/, ""); print $NF}') && \
    cp -L "$interp" /sbcl-libs/$(basename "$interp") && \
    ls -la /sbcl-libs/

# ── Stage 2: Build Go HTTPS secret proxy ─────────────────────────────
FROM golang:1.22-alpine AS proxy-build

COPY proxy/ /build/
RUN cd /build && CGO_ENABLED=0 go build -ldflags='-s -w' -o /sq-secret-proxy-https .

# ── Stage 3: Build guest agent (Firecracker VM support) ──────────────
# Currently shell-based; stage is ready for a static C agent when needed.
FROM alpine:3.21 AS guest-build

RUN apk add --no-cache coreutils
COPY vm/ /guest/
RUN chmod +x /guest/*

# ── Stage 4: Alpine runtime ──────────────────────────────────────────
FROM alpine:3.21

# glibc runtime for SBCL (built on Ubuntu, links glibc).
# SBCL's save-lisp-and-die binary links glibc; Alpine uses musl.
# Copying the exact libs from the build stage avoids compat layer issues.
COPY --from=sbcl-build /sbcl-libs/ /lib/

# Runtime dependencies:
#   libev       — Woo HTTP server event loop (SBCL FFI)
#   libffi      — CFFI foreign function calls
#   squashfs-tools — mksquashfs/unsquashfs for module layers
#   iproute2    — ip command for network namespace veth setup
#   iptables    — NAT + egress filtering for sandboxes
#   jq          — JSON processing in shell scripts (sq-ctl, sq-init)
#   util-linux  — mount, losetup (squashfs loop devices)
#   socat       — Firecracker vsock bridge
#   openssl     — proxy CA certificate generation
#   curl/wget   — S3 module download, health checks
RUN apk add --no-cache \
    libev \
    libffi \
    jq \
    squashfs-tools \
    util-linux \
    curl wget \
    coreutils \
    socat \
    openssl \
    busybox-extras \
    iproute2 \
    iptables \
    && apk add --no-cache aws-cli 2>/dev/null || true \
    && apk add --no-cache tailscale 2>/dev/null || true

# SBCL daemon binary
COPY --from=sbcl-build /build/squashd /app/bin/squashd

# Go secret proxy
COPY --from=proxy-build /sq-secret-proxy-https /app/bin/sq-secret-proxy-https

# Guest agent (Firecracker VM)
COPY --from=guest-build /guest/ /app/vm/

# Shell scripts + static assets
COPY bin/         /app/bin/
COPY cgi-bin/     /app/cgi-bin/
COPY static/      /app/static/
COPY entrypoint.sh /app/

RUN chmod +x /app/bin/* /app/cgi-bin/* /app/cgi-bin/api/* /app/entrypoint.sh

VOLUME /data
EXPOSE 8080

ENV SQUASH_DATA=/data \
    SQUASH_PORT=8080 \
    SQUASH_BACKEND=chroot \
    SQUASH_AUTH_TOKEN="" \
    SQUASH_S3_BUCKET="" \
    SQUASH_S3_ENDPOINT="" \
    SQUASH_S3_REGION="us-east-1" \
    SQUASH_S3_PREFIX="" \
    SQUASH_EPHEMERAL="" \
    SQUASH_UPPER_LIMIT_MB=512 \
    SQUASH_MAX_SANDBOXES=100 \
    SQUASH_PROXY_HTTPS="" \
    TAILSCALE_AUTHKEY="" \
    TAILSCALE_HOSTNAME="squash"

WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
