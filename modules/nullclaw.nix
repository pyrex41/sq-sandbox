{ pkgs, lib }:
let
  pkgsMusl = import nixpkgs {
    inherit (pkgs) system;
    crossSystem = { libc = &quot;musl&quot;; };
  };
  src = pkgs.fetchgit {
    url = &quot;https://github.com/nullclaw/nullclaw&quot;;
    rev = &quot;main&quot;;
    hash = &quot;sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=&quot;; # TODO: nix-prefetch
  };
in lib.mkSquashfsModule {
  name = &quot;200-nullclaw&quot;;
  buildScript = ''
    export HOME=$TMPDIR
    export XDG_CACHE_HOME=$TMPDIR/.cache
    mkdir -p $rootfs/usr/local/bin
    cd ${src}
    zig=${pkgs.zig}/bin/zig build \\
      -Doptimize=ReleaseSmall \\
      -Dtarget=aarch64-linux-musl \\
      -Dsqlite-include=${pkgsMusl.sqlite.dev}/include \\
      -Dsqlite-lib=${pkgsMusl.sqlite.out}/lib
    cp zig-out/bin/nullclaw $rootfs/usr/local/bin/
    $rootfs/usr/local/bin/nullclaw --version || true
  '';
}