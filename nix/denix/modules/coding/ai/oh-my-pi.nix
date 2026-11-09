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
      package = packageOption (
        # In case nixpkgs adds a different package.
        assert (pkgs.omp.meta.homepage == "https://omp.sh");
        pkgs.omp
      );

      settings = {
        models = {
          providers = attrsOfOption (submodule {
            options = {
              baseUrl = noNullDefault (strOption null);
              authHeader = boolOption false;
            };
          }) { };
        };

        config = attrsOption { };
      };
    };

  home.ifEnabled =
    { cfg, ... }:
    let
      inherit (pkgs.writers) writeYAML;
    in
    {
      home.packages = [
        cfg.package
      ];

      home.file = {
        ".omp/agent/models.yml".source = writeYAML "models.yml" {
          providers = cfg.settings.models.providers;
        };
        ".omp/agent/config.yml".source = writeYAML "config.yml" cfg.settings.config;

        # Extensions
        # ".omp/agent/extensions/my-ext.ts".source = writeYAML "extensions/my-ext.ts" { };
      };
    };
}
