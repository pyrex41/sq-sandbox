# Module aggregator — imports all module definitions and exposes them as packages.
{ pkgs }:
let
  lib = import ./lib-squashfs.nix { inherit pkgs; };
  callModule = path: import path { inherit pkgs lib; };

  python = callModule ../modules/python.nix;
  nodejs = callModule ../modules/nodejs.nix;
in {
  module-base-alpine = callModule ../modules/base-alpine.nix;
  module-python312 = python.python312;
  module-nodejs22 = nodejs.nodejs22;
  module-golang = callModule ../modules/golang.nix;
  module-tailscale = callModule ../modules/tailscale.nix;
}
