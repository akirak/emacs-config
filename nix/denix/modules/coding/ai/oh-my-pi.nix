{
  delib,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "oh-my-pi";

  options =
    with delib;
    moduleOptions {
      # Not using on desktop for now.
      enable = boolOption false;

      # currently only available from llm-agents.nix
      package = packageOption pkgs.llm-agents.omp;
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.packages = [
        cfg.package
      ];
    };
}
