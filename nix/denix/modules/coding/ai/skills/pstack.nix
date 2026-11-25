{
  delib,
  pkgs,
  lib,
  host,
  ...
}:
delib.module {
  name = "pstack";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;

      src = pathOption pkgs.agent-plugins.pstack-claude;

      pi = {
        enable = boolOption true;
      };
    };

  myconfig.ifEnabled = { cfg, ... }: {
    pi = lib.mkIf cfg.pi.enable {
      extensions.pstack.source = cfg.src;
      skills.pstack.source = cfg.src + "/plugins/pstack/skills";
    };
  };
}
