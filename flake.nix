{
  description = "sq-sandbox -- composable container sandboxes from squashfs layers";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [
      "x86_64-linux" "aarch64-linux" "aarch64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        isLinux = pkgs.stdenv.isLinux;
      in {
        devShells = {
          default = pkgs.mkShell {
            name = "sq-sandbox";
            packages = with pkgs; [ jq curl git shellcheck ];
          };

          shell = pkgs.mkShell {
            name = "sq-sandbox-shell";
            packages = with pkgs; [ shellcheck jq curl ]
              ++ pkgs.lib.optionals isLinux [ pkgs.busybox ];
          };

          rust = pkgs.mkShell {
            name = "sq-sandbox-rust";
            packages = with pkgs; [ rustc cargo rust-analyzer clippy rustfmt ];
            # NO openssl, NO pkg-config -- uses rustls
          };

          odin = pkgs.mkShell {
            name = "sq-sandbox-odin";
            packages = with pkgs; [ odin ];
          };

          zig = pkgs.mkShell {
            name = "sq-sandbox-zig";
            packages = with pkgs; [ zig zls ];
          };

          janet = pkgs.mkShell {
            name = "sq-sandbox-janet";
            packages = with pkgs; [ janet jpm ];
          };

          cl = pkgs.mkShell {
            name = "sq-sandbox-cl";
            packages = with pkgs; [ sbcl ]
              ++ pkgs.lib.optionals isLinux [ pkgs.libev pkgs.libffi ];
          };
        };

        packages =
          # Squashfs modules (Linux only)
          (pkgs.lib.optionalAttrs isLinux
            (import ./nix/modules.nix { inherit pkgs; })
          )
          # Daemon builds + proxy (Linux only)
          // pkgs.lib.optionalAttrs isLinux {

            # ── Go HTTPS proxy ──────────────────────────────────────────
            sq-secret-proxy = pkgs.stdenv.mkDerivation {
              pname = "sq-secret-proxy-https";
              version = "0.1.0";
              src = ./shared/proxy;
              nativeBuildInputs = [ pkgs.go_1_22 ];
              buildPhase = ''
                export HOME=$TMPDIR
                export GOCACHE=$TMPDIR/go-cache
                export GOPATH=$TMPDIR/go
                CGO_ENABLED=0 go build -ldflags='-s -w' -o sq-secret-proxy-https .
              '';
              installPhase = ''
                mkdir -p $out/bin
                cp sq-secret-proxy-https $out/bin/
              '';
            };

            # ── Rust daemon ─────────────────────────────────────────────
            squashd-rust = pkgs.rustPlatform.buildRustPackage {
              pname = "squashd-rust";
              version = "0.1.0";
              src = ./impl/rust;
              cargoLock.lockFile = ./impl/rust/Cargo.lock;
              # Uses rustls — no native TLS deps needed
              buildInputs = [];
              nativeBuildInputs = [];
            };

            # ── Odin daemon ─────────────────────────────────────────────
            squashd-odin = pkgs.stdenv.mkDerivation {
              pname = "squashd-odin";
              version = "0.1.0";
              src = ./impl/odin;
              nativeBuildInputs = [ pkgs.odin ];
              buildPhase = ''
                odin build src -out:squashd -target:linux_amd64 -o:speed
              '';
              installPhase = ''
                mkdir -p $out/bin
                cp squashd $out/bin/
              '';
            };

            # ── Zig daemon ──────────────────────────────────────────────
            squashd-zig = pkgs.stdenv.mkDerivation {
              pname = "squashd-zig";
              version = "0.1.0";
              src = ./impl/zig;
              nativeBuildInputs = [ pkgs.zig ];
              dontConfigure = true;
              dontInstall = true;
              buildPhase = ''
                export XDG_CACHE_HOME=$TMPDIR/.cache
                zig build -Doptimize=ReleaseSafe --prefix $out
              '';
            };

            # ── Janet daemon (interpreted — wrapper + source) ───────────
            squashd-janet = pkgs.writeShellApplication {
              name = "squashd";
              runtimeInputs = [ pkgs.janet ];
              text = ''
                exec janet ${./impl/janet/main.janet} "$@"
              '';
            };

            # ── Shell daemon (v3 multi-process entrypoint) ──────────────
            squashd-shell = pkgs.writeShellApplication {
              name = "squashd";
              runtimeInputs = with pkgs; [ busybox jq curl ];
              text = ''
                exec ${./shared/bin/start-api} "$@"
              '';
            };

            # ── CL daemon (SBCL + Quicklisp — impure build) ────────────
            # NOTE: This requires __impure = true for Quicklisp network
            # access. Proper Nix-native CL build is a future refinement.
            squashd-cl = pkgs.stdenv.mkDerivation {
              pname = "squashd-cl";
              version = "4.0.0";
              src = ./impl/cl;
              __impure = true;
              nativeBuildInputs = with pkgs; [ sbcl curl which ];
              buildInputs = with pkgs; [ libev libffi ];
              buildPhase = ''
                export HOME=$TMPDIR
                # Install Quicklisp
                curl -sSfL -o $TMPDIR/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
                sbcl --noinform --non-interactive \
                  --load $TMPDIR/quicklisp.lisp \
                  --eval '(quicklisp-quickstart:install :path "'$TMPDIR'/quicklisp/")'

                # Build standalone binary
                sbcl --noinform --non-interactive \
                  --load $TMPDIR/quicklisp/setup.lisp \
                  --eval '(push (truename ".") asdf:*central-registry*)' \
                  --eval '(ql:quickload "squashd")' \
                  --eval '(sb-ext:save-lisp-and-die "squashd" :toplevel #'"'"'squashd:main :executable t :compression 9)'
              '';
              installPhase = ''
                mkdir -p $out/bin
                cp squashd $out/bin/
              '';
            };
          };
      }
    );
}
