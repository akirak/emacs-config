{
  delib,
  pkgs,
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
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = with pkgs; [
      dua
      duf
    ];

    programs.bottom = {
      enable = cfg.bottom.enable;

      settings = {
        flags.process_memory_as_value = true;
      };
    };
  };
}
