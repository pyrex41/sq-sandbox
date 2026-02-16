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

        packages = (pkgs.lib.optionalAttrs isLinux
          (import ./nix/modules.nix { inherit pkgs; })
        ) // pkgs.lib.optionalAttrs isLinux {
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
        };
      }
    );
}
