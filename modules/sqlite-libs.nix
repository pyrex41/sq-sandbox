{ pkgs, lib }:
let
  pkgsMusl = import nixpkgs {
    system = &quot;aarch64-linux&quot;;
    crossSystem = {
      config = &quot;aarch64-unknown-linux-musl&quot;;
    };
  };
in lib.mkSquashfsModule {
  name = &quot;090-sqlite-libs&quot;;
  buildScript = ''
    mkdir -p $rootfs/usr/lib
    cp -L ${pkgsMusl.sqlite.out}/lib/*.so* $rootfs/usr/lib/
    chmod 755 $rootfs/usr/lib/*.so*
  '';
}