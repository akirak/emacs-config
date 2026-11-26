{
  delib,
  pkgs,
  lib,
  host,
  ...
}:
delib.module {
  name = "orca";

  options =
    with delib;
    moduleOptions {
      enable = boolOption (host.isDesktop && host.codingFeatured);

      package = packageOption pkgs.orca-ade;
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      cfg.package
    ];
  };
}
