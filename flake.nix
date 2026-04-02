{
  description = "sq-sandbox -- composable container sandboxes from squashfs layers";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Agent sources — flake = false so we get source trees without pulling
    # in their flake dependencies (zig2nix, etc.)
    nullclaw-src = { url = "github:nullclaw/nullclaw"; flake = false; };
    odin-claw-src = { url = "github:pyrex41/odin-claw"; flake = false; };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils, nullclaw-src, odin-claw-src }:
    flake-utils.lib.eachSystem [
      "x86_64-linux" "aarch64-linux" "aarch64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs-unstable = import nixpkgs-unstable { inherit system; };
        isLinux = pkgs.stdenv.isLinux;

        # ── Squashfs modules (base + agent layers) ──────────────────
        modules = pkgs.lib.optionalAttrs isLinux
          (import ./nix/modules.nix {
            inherit pkgs pkgs-unstable nullclaw-src odin-claw-src;
          });

        # ── Go daemon (includes proxy) ────────────────────────────────
        daemons = pkgs.lib.optionalAttrs isLinux {

          squashd = pkgs.stdenv.mkDerivation {
            pname = "squashd";
            version = "0.1.0";
            src = ./impl/go;
            nativeBuildInputs = [ pkgs.go_1_22 ];
            buildPhase = ''
              export HOME=$TMPDIR
              export GOCACHE=$TMPDIR/go-cache
              export GOPATH=$TMPDIR/go
              CGO_ENABLED=0 go build -ldflags='-s -w' -o squashd .
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp squashd $out/bin/
            '';
          };

        };

        # ── Deployment ───────────────────────────────────────────────
        deployment = pkgs.lib.optionalAttrs isLinux {
          sq-sandbox-service = pkgs.writeTextFile {
            name = "sq-sandbox.service";
            destination = "/lib/systemd/system/sq-sandbox.service";
            text = ''
              [Unit]
              Description=sq-sandbox composable sandbox daemon
              After=network.target

              [Service]
              Type=simple
              ExecStart=/usr/local/bin/squashd
              Environment=SQUASH_DATA=/var/lib/sq-sandbox
              Environment=SQUASH_PORT=8080
              Restart=on-failure
              RestartSec=5

              [Install]
              WantedBy=multi-user.target
            '';
          };
        };

        # ── OCI images ────────────────────────────────────────────────
        images = pkgs.lib.optionalAttrs isLinux
          (import ./nix/image.nix { inherit pkgs; self-packages = daemons; });

      in {
        devShells = {
          default = pkgs.mkShell {
            name = "sq-sandbox";
            packages = with pkgs; [ go_1_22 gopls go-tools jq curl git shellcheck ]
              ++ pkgs.lib.optionals isLinux [ squashfuse fuse-overlayfs bubblewrap ];
          };
        };

        packages = modules // daemons // images // deployment;
      }
    );
}
