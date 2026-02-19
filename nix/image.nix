# OCI image construction for sq-sandbox
#
# Each image layers: runtime deps → shared scripts → proxy → daemon binary
# Usage: nix build .#image-rust  →  docker load < result
{ pkgs, self-packages }:
let
  # Common runtime dependencies for all images
  runtimeDeps = with pkgs; [
    squashfsTools    # mksquashfs, unsquashfs
    util-linux       # mount, losetup
    iproute2         # ip (veth, netns)
    iptables         # NAT, egress filtering
    curl
    wget
    jq
    coreutils
    socat            # Firecracker vsock bridge
    openssl          # proxy CA cert generation
    busybox          # httpd, sh, etc.
  ];

  # Additional deps for CL (glibc-linked SBCL)
  clExtraDeps = with pkgs; [
    libev
    libffi
  ];

  shared = ../shared;

  mkImage = { name, daemonPkg, entrypoint ? "entrypoint-v3.sh", extraDeps ? [] }:
    pkgs.dockerTools.buildLayeredImage {
      inherit name;
      tag = "latest";

      contents = runtimeDeps ++ extraDeps ++ [
        daemonPkg
        self-packages.sq-secret-proxy
      ];

      extraCommands = ''
        # Shared scripts
        mkdir -p app/bin app/cgi-bin app/static app/vm
        cp -r ${shared}/bin/* app/bin/
        cp -r ${shared}/cgi-bin/* app/cgi-bin/
        cp -r ${shared}/static/* app/static/
        cp -r ${shared}/vm/* app/vm/
        cp ${shared}/seccomp.json app/
        cp ${shared}/${entrypoint} app/entrypoint.sh
        chmod +x app/entrypoint.sh
      '';

      config = {
        Cmd = [ "/app/entrypoint.sh" ];
        WorkingDir = "/app";
        Env = [
          "PATH=/app/bin:/bin:/usr/bin:/sbin:/usr/sbin"
          "SQUASH_DATA=/data"
        ];
        ExposedPorts = {
          "8080/tcp" = {};
        };
        Volumes = {
          "/data" = {};
        };
      };
    };

in {
  image-rust = mkImage {
    name = "sq-sandbox-rust";
    daemonPkg = self-packages.squashd-rust;
  };

  image-odin = mkImage {
    name = "sq-sandbox-odin";
    daemonPkg = self-packages.squashd-odin;
  };

  image-zig = mkImage {
    name = "sq-sandbox-zig";
    daemonPkg = self-packages.squashd-zig;
  };

  image-janet = mkImage {
    name = "sq-sandbox-janet";
    daemonPkg = self-packages.squashd-janet;
  };

  image-shell = mkImage {
    name = "sq-sandbox-shell";
    daemonPkg = self-packages.squashd-shell;
  };

  image-cl = mkImage {
    name = "sq-sandbox-cl";
    daemonPkg = self-packages.squashd-cl;
    entrypoint = "entrypoint-v4.sh";
    extraDeps = clExtraDeps;
  };
}
