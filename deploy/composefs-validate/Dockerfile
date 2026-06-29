# TEMPORARY — scratch image to validate the composefs LayerStore on a Fly
# machine (Fly guest kernel ~6.12). Same as test/Dockerfile.composefs but keeps
# the machine alive (CMD sleep) so the validation script can be driven over SSH.
FROM fedora:41

RUN dnf install -y --setopt=install_weak_deps=False \
        golang composefs erofs-utils fsverity-utils util-linux kmod \
    && dnf clean all

# Fedora ships Go 1.24 with GOTOOLCHAIN=local; the module needs 1.25, so allow
# the toolchain to be auto-fetched.
ENV GOTOOLCHAIN=auto GOSUMDB=sum.golang.org
WORKDIR /src
COPY impl/go/go.mod impl/go/go.sum ./impl/go/
RUN cd impl/go && go mod download
COPY . .
RUN cd impl/go && go build ./...

CMD ["sleep", "infinity"]
