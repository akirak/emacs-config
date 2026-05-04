{ delib, host, ... }:
delib.module {
  name = "wayland";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;

      display = strOption "wayland-1";
    };

  myconfig.always =
    { cfg, ... }:
    {
      args.shared.enableWayland = cfg.enable;
    };
}
