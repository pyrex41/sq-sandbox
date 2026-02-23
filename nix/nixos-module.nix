{ config, lib, pkgs, ... }:

let
  cfg = config.services.sq-sandbox;
in {
  options.services.sq-sandbox = {
    enable = lib.mkEnableOption "sq-sandbox composable sandbox daemon";

    dataDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/sq-sandbox";
      description = "Directory for sandbox data, modules, and state";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "HTTP API listen port";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.sq-sandbox or null;
      description = "The sq-sandbox package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.sq-sandbox = {
      description = "sq-sandbox composable sandbox daemon";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/squashd";
        Restart = "on-failure";
        RestartSec = 5;
      };

      environment = {
        SQUASH_DATA = cfg.dataDir;
        SQUASH_PORT = toString cfg.port;
      };
    };
  };
}
