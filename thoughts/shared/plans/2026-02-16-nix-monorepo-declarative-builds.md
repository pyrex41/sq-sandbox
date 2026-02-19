# Nix Monorepo: Declarative Reproducible Builds for All 6 Implementations

## Overview

Consolidate all 6 sq-sandbox implementations (shell, Odin, Rust, Zig, Janet, Common Lisp) into a single monorepo with a Nix flake providing: dev shells for each language (macOS + Linux), declarative squashfs module building, per-language daemon builds, and OCI image construction — replacing all Dockerfiles and imperative build scripts.

## Current State Analysis

**6 separate repos** with identical shared infrastructure (`bin/`, `cgi-bin/`, `proxy/`, `vm/`, `entrypoint.sh`, `static/`) duplicated across each. No Nix. No CI. No version pinning on module builds.

### Key Discoveries:
- All 6 repos share identical `bin/sq-mkbase`, `bin/sq-mkmod`, `bin/sq-mkvm`, `cgi-bin/`, `entrypoint.sh`, `static/`
- Dockerfiles differ only in the daemon build stage (+ CL needs glibc shims)
- `sq-mkbase` scrapes HTML for "latest" Alpine minirootfs — no SHA256
- `sq-mkmod` scrapes HTML for latest Python/Node/Go — no SHA256
- `apk add` runs without version pins
- Go HTTPS proxy built identically in every repo (`CGO_ENABLED=0 go build -ldflags='-s -w'`)
- CL build is most complex: Ubuntu build stage → Quicklisp → save-lisp-and-die → copy glibc libs to Alpine

## Desired End State

A single monorepo where:

```bash
# Dev shell for any language
nix develop .#rust      # Rust toolchain + runtime deps
nix develop .#odin      # Odin compiler + runtime deps
nix develop .#cl        # SBCL + Quicklisp + runtime deps

# Build any daemon binary
nix build .#squashd-rust
nix build .#squashd-odin
nix build .#squashd-cl

# Build squashfs modules (reproducible, content-addressable)
nix build .#module-base-alpine    # 000-base-alpine.squashfs
nix build .#module-python312      # 100-python312.squashfs
nix build .#module-nodejs22       # 100-nodejs22.squashfs

# Build OCI images
nix build .#image-rust            # Docker-loadable OCI image
nix build .#image-odin
nix build .#image-shell

# Build everything
nix build .#all-images
```

### Verification:
- `nix flake check` passes on macOS and Linux
- `nix develop .#rust` works on macOS (Rust toolchain, type-checking, non-root tests)
- `nix build .#squashd-rust` produces identical binary on any Linux machine
- `nix build .#module-base-alpine` produces squashfs with SHA256-verified Alpine tarball
- `nix build .#image-rust` produces loadable OCI image (`docker load < result`)
- Two builds on different machines produce byte-identical module squashfs files

## What We're NOT Doing

- Rewriting the shell scripts in `bin/` — they stay as-is, Nix wraps them with dependency paths
- Building Alpine packages from source in Nix — we fetch the minirootfs and use `apk` (pinned)
- NixOS modules or flake-parts — plain flake.nix with lib functions, keeps it simple
- Nix-based Firecracker kernel build — we `fetchurl` the pre-built kernel binary
- Replacing jpm/quicklisp with Nix package management — each language keeps its native deps

## Implementation Approach

The monorepo has three layers:
1. **Shared Nix lib** (`nix/`) — functions for building modules, images, runtime package sets
2. **Shared assets** (`shared/`) — scripts, proxy, static files, VM components
3. **Implementations** (`impl/`) — one directory per language with its source + build

## Monorepo Structure

```
sq-sandbox/
├── flake.nix
├── flake.lock
├── nix/
│   ├── runtime-deps.nix      # Alpine packages for runtime image
│   ├── proxy.nix              # Go HTTPS proxy builder
│   ├── modules.nix            # Squashfs module derivations
│   ├── guest.nix              # Firecracker guest components
│   ├── image.nix              # OCI image builder function
│   └── dev-shells.nix         # Per-language dev shell definitions
├── shared/
│   ├── bin/                   # sq-mkbase, sq-mkmod, sq-mkvm, etc.
│   ├── cgi-bin/               # CGI scripts
│   ├── proxy/                 # Go HTTPS proxy source
│   ├── vm/                    # Firecracker guest init + vsock handler
│   ├── static/                # Web UI assets
│   └── entrypoint.sh
├── impl/
│   ├── shell/                 # (empty — uses shared/ CGI directly)
│   ├── odin/
│   │   ├── src/
│   │   └── Makefile
│   ├── rust/
│   │   ├── src/
│   │   ├── guest/             # Firecracker guest agent (Rust)
│   │   ├── Cargo.toml
│   │   └── Cargo.lock
│   ├── zig/
│   │   ├── src/
│   │   └── build.zig
│   ├── janet/
│   │   ├── src/
│   │   └── project.janet
│   └── cl/
│       ├── src/
│       ├── squashd.asd
│       └── squashd-test.asd
├── modules/                   # Declarative module definitions
│   ├── base-alpine.nix
│   ├── python.nix
│   ├── nodejs.nix
│   ├── golang.nix
│   └── tailscale.nix
└── thoughts/
```

---

## Phase 1: Monorepo Consolidation + Flake Skeleton

### Overview
Merge all 6 repos into one monorepo. Create the basic flake.nix that builds only the shared components (Go proxy). Extract shared files into `shared/`.

### Changes Required:

#### 1. Create monorepo structure
Move each implementation's source into `impl/<lang>/`. Move shared infrastructure into `shared/`.

```bash
# From current sq-sandbox repo
mkdir -p impl/shell impl/rust shared
mv bin/ cgi-bin/ static/ entrypoint.sh proxy/ vm/ shared/
mv src/ Cargo.toml Cargo.lock impl/rust/
# Copy guest/ to impl/rust/guest/ if it exists

# From sq-sandbox-odin
cp -r ../sq-sandbox-odin/src impl/odin/src
cp ../sq-sandbox-odin/Makefile impl/odin/

# From sq-sandbox-zig
cp -r ../sq-sandbox-zig/src impl/zig/src
cp ../sq-sandbox-zig/build.zig impl/zig/

# From sq-sandbox-janet
cp -r ../sq-sandbox-janet/src impl/janet/src
cp ../sq-sandbox-janet/project.janet impl/janet/

# From sq-sandbox-cl
cp -r ../sq-sandbox-cl/src impl/cl/src
cp ../sq-sandbox-cl/squashd.asd impl/cl/
cp ../sq-sandbox-cl/squashd-test.asd impl/cl/
```

#### 2. Create flake.nix skeleton

**File**: `flake.nix`

```nix
{
  description = "sq-sandbox — composable container sandboxes from squashfs layers";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Language-specific
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";

    # OCI images
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, crane, nix2container }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        isLinux = pkgs.stdenv.isLinux;
        isDarwin = pkgs.stdenv.isDarwin;

        # Shared Nix library
        runtimeDeps = import ./nix/runtime-deps.nix { inherit pkgs; };
        proxyBuilder = import ./nix/proxy.nix { inherit pkgs; };
        moduleBuilder = import ./nix/modules.nix { inherit pkgs; };
        devShellDefs = import ./nix/dev-shells.nix { inherit pkgs runtimeDeps; };

      in {
        # Dev shells (all platforms)
        devShells = devShellDefs;

        packages = lib.optionalAttrs isLinux (
          let
            imageBuilder = import ./nix/image.nix {
              inherit pkgs nix2container runtimeDeps proxyBuilder;
            };
          in {
            # Shared components
            sq-secret-proxy = proxyBuilder.proxy;

            # Squashfs modules
            module-base-alpine = moduleBuilder.baseAlpine;
            module-python312 = moduleBuilder.python312;
            module-nodejs22 = moduleBuilder.nodejs22;
            module-golang = moduleBuilder.golang;
            module-tailscale = moduleBuilder.tailscale;

            # Per-implementation builds (added in Phase 4)
            # squashd-rust = ...;
            # squashd-odin = ...;

            # OCI images (added in Phase 5)
            # image-rust = ...;
            # image-odin = ...;
          }
        );
      }
    );
}
```

#### 3. Create nix/runtime-deps.nix

Runtime packages needed inside the OCI image (mirrors Dockerfile `apk add`):

```nix
{ pkgs }:
{
  # Packages needed at runtime inside the container
  runtimePackages = with pkgs; [
    jq
    squashfs-tools    # mksquashfs
    util-linux        # mount, losetup
    curl
    wget
    coreutils
    socat
    openssl
    iproute2          # ip command
    iptables
    busybox           # httpd for shell implementation
  ];

  # Packages that are nice-to-have (optional)
  optionalPackages = with pkgs; [
    awscli2           # S3 integration
    tailscale         # VPN
  ];
}
```

#### 4. Create nix/proxy.nix

```nix
{ pkgs }:
{
  proxy = pkgs.buildGoModule {
    pname = "sq-secret-proxy-https";
    version = "0.1.0";
    src = ../shared/proxy;
    vendorHash = null; # Update after first build or use goModules
    CGO_ENABLED = 0;
    ldflags = [ "-s" "-w" ];
  };
}
```

### Success Criteria:

#### Automated Verification:
- [ ] `nix flake check` passes
- [ ] `nix develop` opens a shell with basic tools on macOS and Linux
- [ ] `nix build .#sq-secret-proxy` produces the Go proxy binary (Linux only)
- [ ] All 6 implementations' source files exist under `impl/`
- [ ] `shared/bin/sq-mkbase` exists and is executable

#### Manual Verification:
- [ ] Verify each implementation's source compiles (outside Nix) from the monorepo paths
- [ ] Verify `shared/` scripts reference correct relative paths

**Implementation Note**: After completing this phase and all automated verification passes, pause here for confirmation before proceeding.

---

## Phase 2: Per-Language Dev Shells

### Overview
Create `nix develop .#<lang>` shells for all 6 implementations. macOS gets the toolchain for editing/type-checking. Linux gets full runtime deps.

### Changes Required:

#### 1. Create nix/dev-shells.nix

**File**: `nix/dev-shells.nix`

```nix
{ pkgs, runtimeDeps }:
let
  isLinux = pkgs.stdenv.isLinux;

  # Common tools for all dev shells
  commonPackages = with pkgs; [
    jq
    curl
    git
  ] ++ (if isLinux then runtimeDeps.runtimePackages else []);

  mkDevShell = { name, packages }: pkgs.mkShell {
    inherit name;
    buildInputs = commonPackages ++ packages;
    shellHook = ''
      echo "sq-sandbox dev shell: ${name}"
      ${if isLinux then ''
        echo "  Runtime deps available (Linux)"
      '' else ''
        echo "  Toolchain only (macOS — runtime requires Linux)"
      ''}
    '';
  };

in {
  # Shell implementation (no language toolchain needed)
  shell = mkDevShell {
    name = "sq-sandbox-shell";
    packages = with pkgs; [ shellcheck ];
  };

  # Odin
  odin = mkDevShell {
    name = "sq-sandbox-odin";
    packages = with pkgs; [ odin ols ];
  };

  # Rust
  rust = mkDevShell {
    name = "sq-sandbox-rust";
    packages = with pkgs; [
      rustc cargo rust-analyzer clippy rustfmt
      pkg-config openssl.dev
    ];
  };

  # Zig
  zig = mkDevShell {
    name = "sq-sandbox-zig";
    packages = with pkgs; [ zig zls ];
  };

  # Janet
  janet = mkDevShell {
    name = "sq-sandbox-janet";
    packages = with pkgs; [ janet ];
  };

  # Common Lisp
  cl = mkDevShell {
    name = "sq-sandbox-cl";
    packages = with pkgs; [
      sbcl
      libev   # Woo HTTP server
      libffi  # CFFI
    ];
    # Quicklisp bootstrapped via shellHook
  };

  # Default shell
  default = mkDevShell {
    name = "sq-sandbox";
    packages = with pkgs; [ shellcheck ];
  };
}
```

### Success Criteria:

#### Automated Verification:
- [ ] `nix develop .#shell` opens shell with `shellcheck` available
- [ ] `nix develop .#odin` opens shell with `odin version` working
- [ ] `nix develop .#rust` opens shell with `cargo --version` working
- [ ] `nix develop .#zig` opens shell with `zig version` working
- [ ] `nix develop .#janet` opens shell with `janet -v` working
- [ ] `nix develop .#cl` opens shell with `sbcl --version` working
- [ ] All of the above work on both macOS (aarch64-darwin) and Linux (x86_64-linux)

#### Manual Verification:
- [ ] `nix develop .#odin` → `cd impl/odin && odin check src` succeeds
- [ ] `nix develop .#rust` → `cd impl/rust && cargo check` succeeds
- [ ] `nix develop .#zig` → `cd impl/zig && zig build --help` works
- [ ] `nix develop .#cl` → `cd impl/cl && sbcl --eval '(require :asdf)'` works

**Implementation Note**: Pause for manual verification on both macOS and Linux.

---

## Phase 3: Declarative Squashfs Module Building

### Overview
Replace `sq-mkbase` and `sq-mkmod` preset builds with Nix derivations. Every module is a derivation with pinned source URLs and SHA256 hashes. Outputs are deterministic squashfs files.

### Changes Required:

#### 1. Create nix/modules.nix

**File**: `nix/modules.nix`

```nix
{ pkgs }:
let
  inherit (pkgs) lib fetchurl runCommand squashfs-tools-ng;

  # Helper: build a squashfs module from a rootfs directory
  mkSquashfsModule = { name, rootfsBuilder, ... }:
    runCommand "${name}.squashfs" {
      nativeBuildInputs = [ pkgs.squashfsTools ];
    } ''
      rootfs=$(mktemp -d)
      ${rootfsBuilder rootfs}
      mksquashfs "$rootfs" "$out" \
        -comp zstd -Xcompression-level 15 -b 128K \
        -noappend -quiet -no-xattrs -no-exports \
        -all-root \
        -mkfs-time 0 -all-time 0
    '';

  # Helper: fetch and extract a tarball into a rootfs
  fetchAndExtract = { url, hash, rootfs, stripComponents ? 0, destDir ? "" }:
    let strip = if stripComponents > 0 then "--strip-components=${toString stripComponents}" else "";
        dest = if destDir != "" then "${rootfs}${destDir}" else rootfs;
    in ''
      mkdir -p "${dest}"
      tar xf ${fetchurl { inherit url hash; }} -C "${dest}" ${strip}
    '';

in {
  # ── 000-base-alpine ────────────────────────────────────────────────
  # Minimal Alpine rootfs — NO apk add (that happens at sandbox create time)
  # This matches the minirootfs tarball exactly, compressed as squashfs.
  baseAlpine = mkSquashfsModule {
    name = "000-base-alpine";
    rootfsBuilder = rootfs: ''
      ${fetchAndExtract {
        url = "https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/x86_64/alpine-minirootfs-3.21.3-x86_64.tar.gz";
        hash = "sha256-PLACEHOLDER";  # nix-prefetch-url to get this
        inherit rootfs;
      }}
      # Configure APK repos for sandbox use
      mkdir -p ${rootfs}/etc/apk
      cat > ${rootfs}/etc/apk/repositories <<'REPOS'
      https://dl-cdn.alpinelinux.org/alpine/v3.21/main
      https://dl-cdn.alpinelinux.org/alpine/v3.21/community
      REPOS
    '';
  };

  # ── 100-python312 ─────────────────────────────────────────────────
  # Python 3.12 from python-build-standalone (pre-built, no compilation)
  python312 = mkSquashfsModule {
    name = "100-python312";
    rootfsBuilder = rootfs: ''
      ${fetchAndExtract {
        url = "https://github.com/indygreg/python-build-standalone/releases/download/20241002/cpython-3.12.7+20241002-x86_64-unknown-linux-gnu-install_only_stripped.tar.gz";
        hash = "sha256-PLACEHOLDER";
        inherit rootfs;
        stripComponents = 1;
        destDir = "/usr/local";
      }}
      # ensurepip
      ${rootfs}/usr/local/bin/python3 -m ensurepip 2>/dev/null || true
    '';
  };

  # ── 100-nodejs22 ──────────────────────────────────────────────────
  nodejs22 = mkSquashfsModule {
    name = "100-nodejs22";
    rootfsBuilder = rootfs: ''
      ${fetchAndExtract {
        url = "https://nodejs.org/dist/v22.13.1/node-v22.13.1-linux-x64.tar.xz";
        hash = "sha256-PLACEHOLDER";
        inherit rootfs;
        stripComponents = 1;
        destDir = "/usr/local";
      }}
    '';
  };

  # ── 100-golang ────────────────────────────────────────────────────
  golang = mkSquashfsModule {
    name = "100-golang";
    rootfsBuilder = rootfs: ''
      ${fetchAndExtract {
        url = "https://go.dev/dl/go1.23.6.linux-amd64.tar.gz";
        hash = "sha256-PLACEHOLDER";
        inherit rootfs;
        destDir = "/usr/local";
      }}
      # Create profile script for PATH
      mkdir -p ${rootfs}/etc/profile.d
      cat > ${rootfs}/etc/profile.d/go.sh <<'EOF'
      export PATH=/usr/local/go/bin:$HOME/go/bin:$PATH
      export GOPATH=$HOME/go
      EOF
    '';
  };

  # ── 200-tailscale ─────────────────────────────────────────────────
  tailscale = mkSquashfsModule {
    name = "200-tailscale";
    rootfsBuilder = rootfs: ''
      tmp=$(mktemp -d)
      tar xzf ${fetchurl {
        url = "https://pkgs.tailscale.com/stable/tailscale_1.78.3_amd64.tgz";
        hash = "sha256-PLACEHOLDER";
      }} -C "$tmp"
      mkdir -p ${rootfs}/usr/local/bin ${rootfs}/usr/local/sbin
      cp "$tmp"/tailscale_*/tailscale ${rootfs}/usr/local/bin/
      cp "$tmp"/tailscale_*/tailscaled ${rootfs}/usr/local/sbin/
    '';
  };
}
```

**Note on SHA256 hashes**: All `hash = "sha256-PLACEHOLDER"` values must be filled in. Run `nix-prefetch-url <url>` for each URL, then convert to SRI format with `nix hash to-sri --type sha256 <hash>`.

#### 2. Pin versions in modules/versions.json

**File**: `modules/versions.json` — single source of truth for module versions

```json
{
  "alpine": {
    "version": "3.21.3",
    "arch": "x86_64",
    "url": "https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/x86_64/alpine-minirootfs-3.21.3-x86_64.tar.gz",
    "sha256": "sha256-PLACEHOLDER"
  },
  "python": {
    "version": "3.12.7",
    "release": "20241002",
    "url": "https://github.com/indygreg/python-build-standalone/releases/download/20241002/cpython-3.12.7+20241002-x86_64-unknown-linux-gnu-install_only_stripped.tar.gz",
    "sha256": "sha256-PLACEHOLDER"
  },
  "nodejs": {
    "version": "22.13.1",
    "url": "https://nodejs.org/dist/v22.13.1/node-v22.13.1-linux-x64.tar.xz",
    "sha256": "sha256-PLACEHOLDER"
  },
  "golang": {
    "version": "1.23.6",
    "url": "https://go.dev/dl/go1.23.6.linux-amd64.tar.gz",
    "sha256": "sha256-PLACEHOLDER"
  },
  "tailscale": {
    "version": "1.78.3",
    "url": "https://pkgs.tailscale.com/stable/tailscale_1.78.3_amd64.tgz",
    "sha256": "sha256-PLACEHOLDER"
  }
}
```

#### 3. Keep sq-mkbase/sq-mkmod as runtime fallback

The shell scripts in `shared/bin/` stay as-is for runtime use (inside containers without Nix). The Nix derivations are the primary build path; the scripts are the fallback for environments where Nix isn't available.

### Success Criteria:

#### Automated Verification:
- [ ] `nix build .#module-base-alpine` produces a squashfs file
- [ ] `nix build .#module-python312` produces a squashfs file
- [ ] `nix build .#module-nodejs22` produces a squashfs file
- [ ] `nix build .#module-golang` produces a squashfs file
- [ ] `nix build .#module-tailscale` produces a squashfs file
- [ ] Two sequential builds of `module-base-alpine` produce identical output (byte-for-byte)
- [ ] `unsquashfs -l result` shows expected directory structure for each module

#### Manual Verification:
- [ ] Mount a Nix-built `000-base-alpine.squashfs` and verify it contains `/etc/apk/repositories`
- [ ] Mount `100-python312.squashfs` and verify `/usr/local/bin/python3` exists
- [ ] Run a sandbox using Nix-built modules and verify `python3 --version` works inside

**Implementation Note**: The SHA256 hashes need to be populated by running `nix-prefetch-url` for each URL. This is a one-time manual step per version bump.

---

## Phase 4: Per-Language Daemon Builds

### Overview
Build the squashd daemon binary for each language via `nix build .#squashd-<lang>`.

### Changes Required:

#### 1. Rust — crane

Add to `flake.nix` packages:

```nix
squashd-rust = let
  craneLib = crane.mkLib pkgs;
  commonArgs = {
    src = craneLib.cleanCargoSource ./impl/rust;
    strictDeps = true;
    buildInputs = with pkgs; [ openssl ];
    nativeBuildInputs = with pkgs; [ pkg-config ];
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
in craneLib.buildPackage (commonArgs // {
  inherit cargoArtifacts;
});
```

#### 2. Odin — stdenv + odin

```nix
squashd-odin = pkgs.stdenv.mkDerivation {
  pname = "squashd-odin";
  version = "0.1.0";
  src = ./impl/odin;
  nativeBuildInputs = [ pkgs.odin ];
  buildPhase = ''
    odin build src -out:squashd -target:linux_amd64 \
      -o:size -no-bounds-check -disable-assert \
      -extra-linker-flags:"-s -Wl,--gc-sections"
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp squashd $out/bin/
  '';
};
```

#### 3. Zig — stdenv + zig

```nix
squashd-zig = pkgs.stdenv.mkDerivation {
  pname = "squashd-zig";
  version = "0.1.0";
  src = ./impl/zig;
  nativeBuildInputs = [ pkgs.zig ];
  dontConfigure = true;
  buildPhase = ''
    zig build -Doptimize=ReleaseSafe
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp zig-out/bin/squashd $out/bin/
  '';
};
```

#### 4. Janet — stdenv + janet

```nix
squashd-janet = pkgs.stdenv.mkDerivation {
  pname = "squashd-janet";
  version = "0.1.0";
  src = ./impl/janet;
  nativeBuildInputs = [ pkgs.janet ];
  buildPhase = ''
    janet -c src/main.janet squashd.jimage
  '';
  installPhase = ''
    mkdir -p $out/bin
    # Wrapper script that runs janet with the compiled image
    cat > $out/bin/squashd <<'WRAPPER'
    #!/bin/sh
    exec janet $out/lib/squashd.jimage "$@"
    WRAPPER
    chmod +x $out/bin/squashd
    mkdir -p $out/lib
    cp squashd.jimage $out/lib/
  '';
};
```

#### 5. Common Lisp — SBCL + Quicklisp

This is the most complex build. SBCL needs glibc (not musl), Quicklisp needs network access during dependency resolution.

```nix
squashd-cl = pkgs.stdenv.mkDerivation {
  pname = "squashd-cl";
  version = "0.1.0";
  src = ./impl/cl;

  nativeBuildInputs = with pkgs; [ sbcl curl cacert ];
  buildInputs = with pkgs; [ libev libffi ];

  # Quicklisp bootstrap + dependency fetch
  # This is a fixed-output derivation concern — may need __impure
  # or pre-vendored quicklisp deps. See implementation note below.
  buildPhase = ''
    export HOME=$TMPDIR
    # Bootstrap Quicklisp
    curl -sO https://beta.quicklisp.org/quicklisp.lisp
    sbcl --non-interactive \
      --load quicklisp.lisp \
      --eval '(quicklisp-quickstart:install :path "'$HOME'/quicklisp")' \
      --quit
    echo '#-quicklisp (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' > $HOME/.sbclrc

    # Build image
    sbcl --non-interactive \
      --eval '(push #P"'$PWD'/" asdf:*central-registry*)' \
      --eval '(ql:quickload "squashd")' \
      --eval '(squashd::build-image :output "'$PWD'/squashd")'
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp squashd $out/bin/
  '';
};
```

**Implementation Note**: The CL build needs network access for Quicklisp. Options:
- Use `__impure = true` (simplest, loses full reproducibility)
- Pre-vendor Quicklisp deps as a separate FOD
- Use `cl-nix-lite` to manage CL deps through Nix

For Phase 4, start with `__impure = true` and refine in a follow-up.

#### 6. Shell — no daemon binary

```nix
squashd-shell = pkgs.writeShellApplication {
  name = "start-api";
  runtimeInputs = with pkgs; [ busybox jq curl ];
  text = builtins.readFile ./shared/bin/start-api;
};
```

### Success Criteria:

#### Automated Verification:
- [ ] `nix build .#squashd-rust` produces binary, `file result/bin/squashd` shows ELF x86_64
- [ ] `nix build .#squashd-odin` produces binary
- [ ] `nix build .#squashd-zig` produces binary
- [ ] `nix build .#squashd-janet` produces janet image + wrapper
- [ ] `nix build .#squashd-cl` produces SBCL image
- [ ] `nix build .#squashd-shell` produces wrapped shell script

#### Manual Verification:
- [ ] `./result/bin/squashd --help` works for each compiled implementation
- [ ] Binary sizes are reasonable (Odin < 5MB, Rust < 20MB, CL < 50MB)

---

## Phase 5: OCI Image Construction

### Overview
Replace all Dockerfiles with nix2container. Each image composes: Alpine runtime deps + shared scripts + daemon binary + Go proxy. Produces OCI images loadable with `docker load`.

### Changes Required:

#### 1. Create nix/image.nix

**File**: `nix/image.nix`

```nix
{ pkgs, nix2container, runtimeDeps, proxyBuilder }:
let
  n2c = nix2container.packages.${pkgs.system}.nix2container;

  # Shared scripts layer (bin/, cgi-bin/, static/, entrypoint.sh)
  sharedScripts = pkgs.runCommand "sq-shared-scripts" {} ''
    mkdir -p $out/app
    cp -r ${../shared/bin} $out/app/bin
    cp -r ${../shared/cgi-bin} $out/app/cgi-bin
    cp -r ${../shared/static} $out/app/static
    cp ${../shared/entrypoint.sh} $out/app/entrypoint.sh
    chmod -R +x $out/app/bin $out/app/cgi-bin $out/app/entrypoint.sh
  '';

  # Function: build OCI image for any implementation
  mkImage = { name, daemonPackage ? null }: n2c.buildImage {
    inherit name;
    tag = "latest";

    config = {
      entrypoint = [ "/app/entrypoint.sh" ];
      WorkingDir = "/app";
      Env = [
        "SQUASH_DATA=/data"
        "SQUASH_PORT=8080"
        "SQUASH_BACKEND=chroot"
        "PATH=/app/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
      ];
      ExposedPorts = { "8080/tcp" = {}; };
      Volumes = { "/data" = {}; };
    };

    layers = [
      # Layer 1: Runtime dependencies
      (n2c.buildLayer {
        deps = runtimeDeps.runtimePackages;
      })

      # Layer 2: Shared scripts
      (n2c.buildLayer {
        deps = [ sharedScripts ];
      })

      # Layer 3: Go proxy
      (n2c.buildLayer {
        deps = [ proxyBuilder.proxy ];
        # Copy to /app/bin/sq-secret-proxy-https
      })
    ] ++ (if daemonPackage != null then [
      # Layer 4: Daemon binary (language-specific)
      (n2c.buildLayer {
        deps = [ daemonPackage ];
      })
    ] else []);
  };

in {
  inherit mkImage;
}
```

#### 2. Add image outputs to flake.nix

```nix
# In flake.nix packages:
image-shell = imageBuilder.mkImage { name = "sq-sandbox-shell"; };
image-odin  = imageBuilder.mkImage { name = "sq-sandbox-odin";  daemonPackage = squashd-odin; };
image-rust  = imageBuilder.mkImage { name = "sq-sandbox-rust";  daemonPackage = squashd-rust; };
image-zig   = imageBuilder.mkImage { name = "sq-sandbox-zig";   daemonPackage = squashd-zig; };
image-janet = imageBuilder.mkImage { name = "sq-sandbox-janet"; daemonPackage = squashd-janet; };
image-cl    = imageBuilder.mkImage { name = "sq-sandbox-cl";    daemonPackage = squashd-cl; };
```

### Success Criteria:

#### Automated Verification:
- [ ] `nix build .#image-shell` produces OCI tarball
- [ ] `nix build .#image-rust` produces OCI tarball
- [ ] `docker load < result` succeeds for each image
- [ ] `docker run --rm sq-sandbox-rust:latest /app/bin/squashd --help` shows usage

#### Manual Verification:
- [ ] `docker run --privileged -p 8080:8080 sq-sandbox-rust:latest` starts and responds to health check
- [ ] Create + exec + destroy sandbox works via API
- [ ] Image sizes are comparable to current Dockerfiles (~120-150MB)

---

## Phase 6: CI with Nix Cache

### Overview
GitHub Actions workflow that builds all 6 implementations, runs checks, and pushes OCI images to ghcr.io. Uses cachix for Nix build caching.

### Changes Required:

#### 1. Create .github/workflows/ci.yml

```yaml
name: CI
on: [push, pull_request]

jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: cachix/install-nix-action@v27
        with:
          extra_nix_config: |
            experimental-features = nix-command flakes
      - uses: cachix/cachix-action@v15
        with:
          name: sq-sandbox
          authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}'
      - run: nix flake check

  build-daemons:
    runs-on: ubuntu-latest
    needs: check
    strategy:
      matrix:
        impl: [shell, odin, rust, zig, janet, cl]
    steps:
      - uses: actions/checkout@v4
      - uses: cachix/install-nix-action@v27
      - uses: cachix/cachix-action@v15
        with:
          name: sq-sandbox
          authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}'
      - run: nix build .#squashd-${{ matrix.impl }}

  build-modules:
    runs-on: ubuntu-latest
    needs: check
    steps:
      - uses: actions/checkout@v4
      - uses: cachix/install-nix-action@v27
      - uses: cachix/cachix-action@v15
        with:
          name: sq-sandbox
          authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}'
      - run: |
          nix build .#module-base-alpine
          nix build .#module-python312
          nix build .#module-nodejs22
          nix build .#module-golang
          nix build .#module-tailscale

  build-images:
    runs-on: ubuntu-latest
    needs: [build-daemons, build-modules]
    strategy:
      matrix:
        impl: [shell, odin, rust, zig, janet, cl]
    steps:
      - uses: actions/checkout@v4
      - uses: cachix/install-nix-action@v27
      - uses: cachix/cachix-action@v15
        with:
          name: sq-sandbox
          authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}'
      - run: nix build .#image-${{ matrix.impl }}
      # Optionally push to ghcr.io on main branch
```

### Success Criteria:

#### Automated Verification:
- [ ] CI passes on push to main
- [ ] All 6 daemon builds succeed
- [ ] All 5 module builds succeed
- [ ] All 6 OCI images build successfully
- [ ] cachix cache populated (subsequent builds faster)

#### Manual Verification:
- [ ] Verify CI times are reasonable (< 30 min with warm cache)
- [ ] Verify cachix cache hit rates after first build

---

## Testing Strategy

### Unit Tests:
- `nix flake check` — flake structure, dev shell evaluation
- Per-language tests run inside `nix develop .#<lang>` shells
- Module builds verified via `unsquashfs -l` output

### Integration Tests:
- Build OCI image → `docker run --privileged` → create sandbox → exec → destroy
- Verify Nix-built modules work as sandbox layers

### Manual Testing Steps:
1. `nix develop .#rust` on macOS — verify `cargo check` works
2. `nix build .#image-rust` on Linux — verify image runs
3. Create sandbox with Nix-built `module-base-alpine` + `module-python312`
4. `exec python3 --version` inside sandbox
5. Verify identical outputs across two clean builds

## Performance Considerations

- **First build**: Cold build of all 6 implementations + modules: ~30-60 minutes
- **Incremental**: With cachix, only changed derivations rebuild: ~2-5 minutes
- **Module builds**: One-time per version pin. Subsequent builds instant (content-addressed)
- **OCI images**: nix2container layer caching means only changed layers rebuild

## Migration Notes

- Old Dockerfiles can remain temporarily for comparison/fallback
- `shared/bin/sq-mkbase` and `sq-mkmod` remain functional for environments without Nix
- Module version bumps: update `modules/versions.json`, run `nix-prefetch-url`, update hashes
- The CL build may need `__impure = true` initially until Quicklisp deps are properly vendored

## References

- Current Dockerfiles: `shared/` (to be moved from each repo)
- Module build scripts: `shared/bin/sq-mkbase`, `shared/bin/sq-mkmod`
- Previous research: `thoughts/shared/research/2026-02-15-ap-homoiconic-architecture.md`
- Nix tooling research: crane (Rust), zig2nix (Zig), nixpkgs odin/janet/sbcl
- nix2container: https://github.com/nlewo/nix2container
