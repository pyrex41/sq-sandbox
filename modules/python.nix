# Python module — python-build-standalone CPython
#
# NO ensurepip during build (needs glibc dynamic linker, absent in Nix sandbox).
# pip bootstrap happens at sandbox creation time.
{ pkgs, lib }:
let
  mkPython = { version, tag, sources }:
    let
      name = "100-python${builtins.replaceStrings ["."] [""] version}";
      src = sources.${pkgs.stdenv.hostPlatform.system}
        or (throw "Unsupported platform for Python ${version}");
      tarball = pkgs.fetchurl { inherit (src) url hash; };
    in
    lib.mkSquashfsModule {
      inherit name;
      buildScript = ''
        mkdir -p "$rootfs/usr/local"
        tar xzf ${tarball} -C "$rootfs/usr/local" --strip-components=1
      '';
    };
in {
  python312 = mkPython {
    version = "3.12.7";
    tag = "20241002";
    sources = {
      x86_64-linux = {
        url = "https://github.com/indygreg/python-build-standalone/releases/download/20241002/cpython-3.12.7+20241002-x86_64-unknown-linux-gnu-install_only_stripped.tar.gz";
        hash = "sha256-XKTojaRNXgdGKkqLWtJ5VtrZ7tz65LHbN6eE2vGCc5Y=";
      };
      aarch64-linux = {
        url = "https://github.com/indygreg/python-build-standalone/releases/download/20241002/cpython-3.12.7+20241002-aarch64-unknown-linux-gnu-install_only_stripped.tar.gz";
        hash = "sha256-XSz2v9ETUbptB6ES/1Y6YhdqP5+QoWnzUPjE10icCNU=";
      };
    };
  };
}
