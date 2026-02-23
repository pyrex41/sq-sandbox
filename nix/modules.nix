# Module aggregator — imports all module definitions and exposes them as packages.
# Base layers (000, 100) + agent layers (200).
{
  pkgs,
  pkgs-unstable,
  nullclaw-src,
  odin-claw-src,
  enableAgents ? true,
}:
let
  lib = import ./lib-squashfs.nix { inherit pkgs; };
  mkSquashfsModule = lib.mkSquashfsModule;
  callModule = path: import path { inherit pkgs lib; };

  python = callModule ../modules/python.nix;
  nodejs = callModule ../modules/nodejs.nix;
  sqlite-libs = callModule ../modules/sqlite-libs.nix;
  module-sqlite-libs = sqlite-libs.squashfs;

  # Agent modules need extra inputs
  agentModules = if enableAgents then {
    module-nullclaw = import ../modules/nullclaw.nix {
      inherit pkgs pkgs-unstable lib nullclaw-src mkSquashfsModule;
    };

    module-odin-claw = import ../modules/odin-claw.nix {
      inherit pkgs lib odin-claw-src mkSquashfsModule;
    };

    module-claude-code = import ../modules/claude-code.nix {
      inherit pkgs lib mkSquashfsModule;
    };
  } else {};

in
{
  # Base layers
  module-base-alpine = callModule ../modules/base-alpine.nix;
  module-python312 = python.python312;
  module-nodejs22 = nodejs.nodejs22;
  module-golang = callModule ../modules/golang.nix;
  module-tailscale = callModule ../modules/tailscale.nix;
  inherit module-sqlite-libs;
} // agentModules
