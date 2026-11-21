{
  delib,
  pkgs,
  host,
  lib,
  ...
}:
delib.module {
  name = "pi";

  options = with delib; moduleOptions {
    enable = boolOption host.codingFeatured;

    package = packageOption pkgs.pi-coding-agent;

    acp = {
      enable = boolOption false;
    };
  };

  home.ifEnabled = {cfg,...}: {
    home.packages = [
      cfg.package
    ] ++ lib.optional cfg.acp.enable pkgs.pi-acp;
  };
}
