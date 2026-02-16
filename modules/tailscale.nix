# Tailscale module — static binaries from pkgs.tailscale.com
{ pkgs, lib }:
let
  version = "1.94.2";
  sources = {
    x86_64-linux = {
      url = "https://pkgs.tailscale.com/stable/tailscale_${version}_amd64.tgz";
      hash = "sha256-xvmaXXdMd4O1aQIYjWnpdW/D3d+wisa+TLJYXz/s3DI=";
    };
    aarch64-linux = {
      url = "https://pkgs.tailscale.com/stable/tailscale_${version}_arm64.tgz";
      hash = "sha256-djAOgIxX63hTCQ1pyL2IBuhjQYYuJEGD9mEfkQV5m7o=";
    };
  };
  src = sources.${pkgs.stdenv.hostPlatform.system}
    or (throw "Unsupported platform for Tailscale ${version}");
  tarball = pkgs.fetchurl { inherit (src) url hash; };
in
lib.mkSquashfsModule {
  name = "200-tailscale";
  buildScript = ''
    mkdir -p "$rootfs/usr/local/bin" "$rootfs/usr/local/sbin"
    tar xzf ${tarball} -C "$TMPDIR"
    cp "$TMPDIR"/tailscale_*/tailscale  "$rootfs/usr/local/bin/"
    cp "$TMPDIR"/tailscale_*/tailscaled "$rootfs/usr/local/sbin/"
  '';
}
