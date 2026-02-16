# Node.js module — pre-built binaries from nodejs.org
{ pkgs, lib }:
let
  mkNodejs = { version, sources }:
    let
      name = "100-nodejs${builtins.head (builtins.splitVersion version)}";
      src = sources.${pkgs.stdenv.hostPlatform.system}
        or (throw "Unsupported platform for Node.js ${version}");
      tarball = pkgs.fetchurl { inherit (src) url hash; };
    in
    lib.mkSquashfsModule {
      inherit name;
      buildScript = ''
        mkdir -p "$rootfs/usr/local"
        tar xJf ${tarball} -C "$rootfs/usr/local" --strip-components=1
      '';
    };
in {
  nodejs22 = mkNodejs {
    version = "22.14.0";
    sources = {
      x86_64-linux = {
        url = "https://nodejs.org/dist/v22.14.0/node-v22.14.0-linux-x64.tar.xz";
        hash = "sha256-abCdulyNywXE5Cc6Q0DbEAWr6v45J+/aK8WySegEN+w=";
      };
      aarch64-linux = {
        url = "https://nodejs.org/dist/v22.14.0/node-v22.14.0-linux-arm64.tar.xz";
        hash = "sha256-CL+/U4utDoy7AmnwFzzKKNcFh0pnoi9gtX2Z3JnjAFA=";
      };
    };
  };
}
