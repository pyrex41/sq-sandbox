{ pkgs }:
let
  inherit (pkgs) lib fetchurl squashfsTools;
in {
  # Build a reproducible squashfs module from a build script.
  #
  # The buildScript receives $rootfs as the directory to populate.
  # Output is a single .squashfs file at $out.
  mkSquashfsModule = { name, buildScript }:
    pkgs.runCommand "${name}.squashfs" {
      nativeBuildInputs = [ squashfsTools pkgs.gnutar pkgs.gzip pkgs.xz ];
      SOURCE_DATE_EPOCH = "0";
    } ''
      rootfs=$(mktemp -d)
      ${buildScript}
      find "$rootfs" -name '.wh.*' -delete 2>/dev/null || true
      mksquashfs "$rootfs" "$out" \
        -comp zstd -Xcompression-level 15 -b 128K \
        -noappend -no-xattrs -no-exports -all-root \
        -no-hardlinks -processors 1 -root-mode 0755 \
        -repro-time 0
      rm -rf "$rootfs"
    '';

  # Helper: fetch a tarball and extract it into $rootfs.
  fetchAndExtract = { url, hash, stripComponents ? 0, destDir ? "" }: let
    src = fetchurl { inherit url hash; };
    strip = lib.optionalString (stripComponents > 0)
      "--strip-components=${toString stripComponents}";
  in ''
    dest="${if destDir != "" then "$rootfs${destDir}" else "$rootfs"}"
    mkdir -p "$dest"
    tar xf ${src} -C "$dest" ${strip}
  '';
}
