{
  delib,
  pkgs,
  host,
  lib,
  ...
}:
delib.module {
  name = "claude-code";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;

      acp = {
        enable = boolOption false;
      };
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      pkgs.claude-code
    ]
    ++ lib.optional cfg.acp.enable pkgs.claude-agent-acp;
  };
}
