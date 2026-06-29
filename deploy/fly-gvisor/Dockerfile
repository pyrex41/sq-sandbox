# sq-sandbox daemon + gVisor backend, for the Fly C/R milestone.
# Build context = repo root. Adds runsc (systrap, no KVM) and bakes a base module
# so a sandbox can be created on Fly without S3.

FROM golang:1.25-alpine AS go-build
WORKDIR /build
COPY impl/go/ .
RUN CGO_ENABLED=0 go build -ldflags='-s -w' -o /squashd .

FROM alpine:3.21
RUN apk add --no-cache \
    squashfs-tools squashfuse fuse-overlayfs fuse3 bubblewrap \
    slirp4netns iproute2 iptables util-linux coreutils \
    jq curl socat openssl bash wget tar

# runsc (gVisor) — matches build arch (Fly default x86_64).
RUN ARCH=$(uname -m) && \
    wget -q "https://storage.googleapis.com/gvisor/releases/release/latest/${ARCH}/runsc" -O /usr/local/bin/runsc && \
    chmod 0755 /usr/local/bin/runsc && runsc --version | head -1

# Daemon (from the gvisor-backend-seam branch) + shell helpers (incl. sq-gvisor).
COPY --from=go-build /squashd /usr/local/bin/squashd
COPY shared/bin/ /usr/local/bin/
RUN chmod +x /usr/local/bin/sq-*

# Bake a base module so createSandbox works without S3. /data is NOT a mounted
# volume here (ephemeral), so this baked content is visible at runtime.
RUN mkdir -p /tmp/base /data/modules && \
    ARCH=$(uname -m) && \
    wget -qO- "https://dl-cdn.alpinelinux.org/alpine/v3.20/releases/${ARCH}/alpine-minirootfs-3.20.3-${ARCH}.tar.gz" \
      | tar -xz -C /tmp/base && \
    mksquashfs /tmp/base /data/modules/000-base-alpine.squashfs \
      -comp zstd -b 128K -noappend -all-root -quiet && \
    rm -rf /tmp/base

EXPOSE 8080
ENV SQUASH_DATA=/data \
    SQUASH_PORT=8080 \
    SQUASH_BACKEND=gvisor \
    SQUASH_AUTH_TOKEN="" \
    SQUASH_MAX_SANDBOXES=20 \
    PATH="/usr/local/bin:$PATH"

CMD ["squashd"]
