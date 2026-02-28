# WireGuard module — wireguard-tools for kernel WireGuard configuration.
# The kernel module (wireguard.ko) is expected in the host/guest kernel;
# this module provides only the userspace `wg` and `wg-quick` tools.
{ pkgs, lib }:
lib.mkSquashfsModule {
  name = "030-wireguard";
  buildScript = ''
    mkdir -p "$rootfs/usr/local/bin" "$rootfs/etc/wireguard"
    cp "${pkgs.wireguard-tools}/bin/wg" "$rootfs/usr/local/bin/"
    cp "${pkgs.wireguard-tools}/bin/wg-quick" "$rootfs/usr/local/bin/"
    chmod 700 "$rootfs/etc/wireguard"
  '';
}
