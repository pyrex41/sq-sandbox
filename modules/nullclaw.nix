# Build nullclaw from source and package as squashfs module.
# Uses pkgs-unstable for Zig 0.15.2+.
{ pkgs, pkgs-unstable, lib, nullclaw-src, mkSquashfsModule }:
let
  # Determine target triple for cross-compilation
  target = if pkgs.stdenv.hostPlatform.isAarch64
    then "aarch64-linux-musl"
    else "x86_64-linux-musl";

  nullclaw = pkgs.stdenv.mkDerivation {
    pname = "nullclaw";
    version = "0-unstable";
    src = nullclaw-src;

    nativeBuildInputs = [ pkgs-unstable.zig ];

    # Zig manages its own cache; point it at a writable directory
    ZIG_GLOBAL_CACHE_DIR = "$TMPDIR/zig-cache";

    buildPhase = ''
      runHook preBuild
      mkdir -p $TMPDIR/zig-cache
      zig build -Doptimize=ReleaseSmall -Dtarget=${target} \
        --global-cache-dir $TMPDIR/zig-cache
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      cp zig-out/bin/nullclaw $out/bin/
      runHook postInstall
    '';

    # Zig handles its own parallelism
    enableParallelBuilding = false;
  };
in
mkSquashfsModule {
  name = "200-nullclaw";
  buildScript = ''
    mkdir -p "$rootfs/usr/local/bin"
    cp ${nullclaw}/bin/nullclaw "$rootfs/usr/local/bin/"

    # Default config template
    mkdir -p "$rootfs/etc/nullclaw"
    cat > "$rootfs/etc/nullclaw/config.json.template" <<'TMPL'
{
  "models": {
    "default_provider": "anthropic",
    "default_model": "anthropic/claude-sonnet-4",
    "providers": [
      {
        "name": "anthropic",
        "api_key": "sq-secret-NULLCLAW_API_KEY"
      }
    ]
  }
}
TMPL
  '';
}
