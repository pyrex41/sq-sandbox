# Go module — official pre-built binaries from go.dev
{ pkgs, lib }:
let
  version = "1.23.6";
  sources = {
    x86_64-linux = {
      url = "https://go.dev/dl/go${version}.linux-amd64.tar.gz";
      hash = "sha256-k3lEHqMQ3gAPM6TcdnvZZucqsoJicOA454ssU8LngC0=";
    };
    aarch64-linux = {
      url = "https://go.dev/dl/go${version}.linux-arm64.tar.gz";
      hash = "sha256-Vhx4Do9KiVXTK/cuRq8LXuXg3r4eRjPfmgN4GHghkgI=";
    };
  };
  src = sources.${pkgs.stdenv.hostPlatform.system}
    or (throw "Unsupported platform for Go ${version}");
  tarball = pkgs.fetchurl { inherit (src) url hash; };
in
lib.mkSquashfsModule {
  name = "100-golang";
  buildScript = ''
    mkdir -p "$rootfs/usr/local"
    tar xzf ${tarball} -C "$rootfs/usr/local"

    # PATH helper
    mkdir -p "$rootfs/etc/profile.d"
    echo 'export PATH=/usr/local/go/bin:$PATH GOPATH=$HOME/go' \
      > "$rootfs/etc/profile.d/go.sh"
  '';
}
