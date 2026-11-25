{
  delib,
  pkgs,
  host,
  lib,
  ...
}:
delib.module {
  name = "pi";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;

      package = packageOption pkgs.pi-coding-agent;

      acp = {
        enable = boolOption false;
      };

      extensions = attrsOfOption (submodule {
        options = {
          source = noNullDefault (pathOption null);
        };
      }) { };

      skills = attrsOfOption (submodule {
        options = {
          source = noNullDefault (pathOption null);
        };
      }) { };
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      cfg.package
    ]
    ++ lib.optional cfg.acp.enable pkgs.pi-acp;

    home.file =
      (lib.mapAttrs' (
        name: extension:
        lib.nameValuePair ".pi/agent/extensions/${name}" {
          inherit (extension) source;
        }
      ) cfg.extensions)
      //
      (lib.mapAttrs' (
        name: extension:
        lib.nameValuePair ".pi/agent/skills/${name}" {
          inherit (extension) source;
        }
      ) cfg.skills);
  };
}
