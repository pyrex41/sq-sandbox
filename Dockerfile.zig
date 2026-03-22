# Dockerfile.zig — Zig daemon + Alpine runtime for Fly.io
# Requires: --privileged or SYS_ADMIN cap (for mount, overlayfs, unshare)

# ── Stage 1: Build Zig daemon ──────────────────────────────────────
FROM alpine:3.21 AS zig-build

RUN apk add --no-cache curl xz jq
RUN curl -L https://ziglang.org/download/0.15.1/zig-x86_64-linux-0.15.1.tar.xz | tar xJ -C /opt
ENV PATH="/opt/zig-x86_64-linux-0.15.1:$PATH"

WORKDIR /build
COPY impl/zig/ .

RUN zig build -Doptimize=ReleaseSafe -Dtarget=x86_64-linux-musl && ls -lh zig-out/bin/

# ── Stage 2: Build Go HTTPS secret proxy ──────────────────────────
FROM golang:1.22-alpine AS proxy-build

COPY shared/proxy/ /build/
RUN cd /build && CGO_ENABLED=0 go build -ldflags='-s -w' -o /sq-secret-proxy-https .

# ── Stage 3: Alpine runtime ───────────────────────────────────────
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

# Zig daemon binary
COPY --from=zig-build /build/zig-out/bin/squashd /usr/local/bin/squashd

# Go secret proxy
COPY --from=proxy-build /sq-secret-proxy-https /usr/local/bin/sq-secret-proxy-https

# Shell helper scripts
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
