{
  delib,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "lean4";

  options =
    with delib;
    moduleOptions {
      enable = boolOption false;

      package = packageOption pkgs.lean4;
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [ cfg.package ];
  };
}
