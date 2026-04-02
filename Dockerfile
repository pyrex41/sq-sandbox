# Dockerfile — Go daemon + Alpine runtime
# Single binary: daemon + HTTPS proxy combined.
# Requires: --privileged or SYS_ADMIN cap (for mount, overlayfs, unshare)

# ── Stage 1: Build Go daemon (includes proxy) ─────────────────────────────
FROM golang:1.22-alpine AS go-build

WORKDIR /build
COPY impl/go/ .
RUN CGO_ENABLED=0 go build -ldflags='-s -w' -o /squashd .

# ── Stage 2: Alpine runtime ───────────────────────────────────────────────
FROM alpine:3.21

RUN apk add --no-cache \
    squashfs-tools \
    squashfuse \
    fuse-overlayfs \
    fuse3 \
    bubblewrap \
    slirp4netns \
    iproute2 \
    iptables \
    util-linux \
    coreutils \
    jq \
    curl \
    socat \
    openssl \
    bash \
    && apk add --no-cache tailscale 2>/dev/null || true

# Single Go binary (daemon + proxy)
COPY --from=go-build /squashd /usr/local/bin/squashd

# Shell helper scripts (sq-exec, sq-mount-layer, sq-mount-overlay, sq-s3, etc.)
COPY shared/bin/ /usr/local/bin/
RUN chmod +x /usr/local/bin/sq-*

# Data volume
VOLUME /data

EXPOSE 8080

ENV SQUASH_DATA=/data \
    SQUASH_PORT=8080 \
    SQUASH_BACKEND=chroot \
    SQUASH_AUTH_TOKEN="" \
    SQUASH_S3_BUCKET="" \
    SQUASH_MAX_SANDBOXES=100 \
    SQUASH_PROXY_HTTPS="" \
    TAILSCALE_AUTHKEY="" \
    PATH="/usr/local/bin:$PATH"

CMD ["squashd"]
