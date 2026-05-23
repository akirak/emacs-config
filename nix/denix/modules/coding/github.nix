{
  delib,
  pkgs,
  lib,
  host,
  ...
}:
delib.module {
  name = "github";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;

      gh.enable = boolOption true;
      gh-enhance.enable = boolOption true;
      zizmor.enable = boolOption true;
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.packages =
        lib.optional cfg.gh.enable pkgs.gh
        ++ lib.optional cfg.gh-enhance.enable pkgs.gh-enhance
        ++ lib.optional cfg.gh-enhance.enable pkgs.zizmor;
    };
}
