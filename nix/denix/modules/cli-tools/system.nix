{
  delib,
  pkgs,
  lib,
  host,
  ...
}:
delib.module {
  name = "cli-tools.system";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.cliFeatured;

      bottom = {
        enable = boolOption true;
      };

      nvtop = {
        enable = boolOption true;
        # There are variants such as amd and nvidia.
        package = packageOption pkgs.nvtopPackages.intel;
      };
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      pkgs.dua
      pkgs.duf
    ]
    ++ lib.optional cfg.nvtop.enable cfg.nvtop.package;

    programs.bottom = {
      enable = cfg.bottom.enable;

      settings = {
        flags.process_memory_as_value = true;
      };
    };
  };
}
