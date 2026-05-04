{
  delib,
  host,
  enableWayland,
  ...
}:
delib.module {
  name = "x11";

  options =
    with delib;
    moduleOptions {
      enable = boolOption (host.guiFeatured && !enableWayland);
      display = strOption ":0";
    };

  myconfig.always =
    { cfg, ... }:
    {
      args.shared.enableX11 = cfg.enable;
    };

  nixos.ifEnabled =
    { cfg, ... }:
    {
      systemd.services.setxkbmap = {
        enable = true;
        after = [ "post-resume.target" ];
        description = "Run setxkbmap";

        script = "/run/current-system/sw/bin/setxkbmap -option ctrl:nocaps";
        environment = {
          DISPLAY = cfg.display;
        };
      };

      services.xserver.xkb.layout = "us";
    };
}
