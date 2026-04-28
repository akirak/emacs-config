{
  delib,
  lib,
  host,
  ...
}:
delib.module {
  name = "npm";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;

      settings = attrsOption {
        "ignore-scripts" = "true";
        "min-release-age" = "3";
      };
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.file.".npmrc".text = lib.pipe cfg.settings [
        (lib.mapAttrsToList (name: value: "${name}=${value}"))
        (lib.concatStringsSep "\n")
      ];
    };
}
