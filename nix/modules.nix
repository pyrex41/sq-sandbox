# Module aggregator — imports all module definitions and exposes them as packages.
{ pkgs, enableNullclaw ? true }:
let
  lib = import ./lib-squashfs.nix { inherit pkgs; };
  callModule = path: import path { inherit pkgs lib; };

  python = callModule ../modules/python.nix;
  nodejs = callModule ../modules/nodejs.nix;
  sqlite-libs = callModule ../modules/sqlite-libs.nix;
  nullclaw = if enableNullclaw then callModule ../modules/nullclaw.nix else {};
  module-sqlite-libs = sqlite-libs.squashfs;
  module-nullclaw = lib.optionalAttrs enableNullclaw { inherit (nullclaw) squashfs; };
in {
  module-base-alpine = callModule ../modules/base-alpine.nix;
  module-python312 = python.python312;
  module-nodejs22 = nodejs.nodejs22;
  module-golang = callModule ../modules/golang.nix;
  module-tailscale = callModule ../modules/tailscale.nix;
}
