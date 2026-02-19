# Alpine 3.21.3 minirootfs — bottom layer for all sandboxes
#
# This does NOT run `apk add` during build (requires chroot + network).
# The base layer contains the raw minirootfs + APK repo config.
# Package installation happens at sandbox creation time.
{ pkgs, lib }:
let
  version = "3.21.3";
  sources = {
    x86_64-linux = {
      url = "https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/x86_64/alpine-minirootfs-${version}-x86_64.tar.gz";
      hash = "sha256-GmlImeQGzlXTIzTEesCy77bAbX6HgQLRhAiSrUTNUjk=";
    };
    aarch64-linux = {
      url = "https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/aarch64/alpine-minirootfs-${version}-aarch64.tar.gz";
      hash = "sha256-6tiks3hnvRnnQX3QeHSOIxLArqNkQD2WdY1j6o/yYeo=";
    };
  };
  src = sources.${pkgs.stdenv.hostPlatform.system}
    or (throw "Unsupported platform for Alpine base: ${pkgs.stdenv.hostPlatform.system}");
  tarball = pkgs.fetchurl { inherit (src) url hash; };
in
lib.mkSquashfsModule {
  name = "000-base-alpine";
  buildScript = ''
    tar xzf ${tarball} -C "$rootfs"

    # APK repo config
    mkdir -p "$rootfs/etc/apk"
    cat > "$rootfs/etc/apk/repositories" <<REPOS
https://dl-cdn.alpinelinux.org/alpine/v3.21/main
https://dl-cdn.alpinelinux.org/alpine/v3.21/community
REPOS

    # Version marker
    echo "${version}" > "$rootfs/etc/alpine-release"
  '';
}
