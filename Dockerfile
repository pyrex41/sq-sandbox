# Squash v3
# The HOST is Alpine (runs the API). SANDBOX GUESTS are any Linux distro
# assembled from squashfs layers.
# Requires: --privileged (for mount, overlayfs, loop, unshare)

FROM alpine:3.21

RUN apk add --no-cache \
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

COPY bin/         /app/bin/
COPY cgi-bin/     /app/cgi-bin/
COPY entrypoint.sh /app/
COPY static/      /app/static/

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
    TAILSCALE_AUTHKEY="" \
    TAILSCALE_HOSTNAME="squash"

WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
