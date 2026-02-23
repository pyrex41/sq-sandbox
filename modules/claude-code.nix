# Package claude-code (npm) as a squashfs module.
# Requires 100-nodejs22 layer in the sandbox at runtime.
{ pkgs, lib, mkSquashfsModule }:
let
  nodejs = pkgs.nodejs_22;
in
mkSquashfsModule {
  name = "200-claude-code";
  buildScript = ''
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"

    # Install claude-code globally under $rootfs/usr/local
    ${nodejs}/bin/npm install -g @anthropic-ai/claude-code \
      --prefix "$rootfs/usr/local" \
      --no-fund --no-audit

    # Init script that sets up claude-code environment
    mkdir -p "$rootfs/usr/local/bin"
    cat > "$rootfs/usr/local/bin/claude-code-init" <<'INIT'
#!/bin/sh
# Source secret proxy env if available
[ -f /etc/profile.d/nanosquash.sh ] && . /etc/profile.d/nanosquash.sh
[ -f /etc/profile.d/squash-secrets.sh ] && . /etc/profile.d/squash-secrets.sh

export ANTHROPIC_API_KEY="''${ANTHROPIC_API_KEY:-sq-secret-ANTHROPIC_API_KEY}"
exec claude "$@"
INIT
    chmod +x "$rootfs/usr/local/bin/claude-code-init"
  '';
}
