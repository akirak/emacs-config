{
  delib,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "deepseek-harness";

  options =
    with delib;
    moduleOptions {
      # Not using on desktop for now.
      enable = boolOption false;

      # currently only available from llm-agents.nix
      package = packageOption pkgs.llm-agents.dsh;

      web = {
        enable = boolOption false;
        port = portOption 8080;
        listenAddress = strOption "127.0.0.1";
        extraArgs = listOfOption str [ ];
      };
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.packages = [
        cfg.package
      ];

      # FIXME: You can't get the auth token from this setup.
      systemd.user.services.dsh-web = lib.mkIf cfg.web.enable {
        Unit = {
          Description = "Serve the DeepSeek Harness browser UI.";
        };

        Install = {
          WantedBy = [ "default.target" ];
        };

        Service = {
          Type = "simple";
          WorkingDirectory = "%h";
          Restart = "on-failure";
          RestartSec = 5;

          ExecStart = "${lib.getExe cfg.package} --profile web --no-open --port ${builtins.toString cfg.web.port} --host ${cfg.web.listenAddress} ${lib.escapeShellArgs cfg.web.extraArgs}";

          Environment = [
            "HOME=%h"
            "PATH=${
              lib.makeBinPath [
                pkgs.git
                pkgs.coreutils
              ]
            }"
          ];
        };
      };
    };
}
