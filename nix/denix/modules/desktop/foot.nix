{
  delib,
  lib,
  homeconfig,
  host,
  ...
}:
delib.module {
  name = "foot";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;

      enableServer = boolOption true;
    };

  home.ifEnabled =
    { myconfig, cfg, ... }:
    {
      programs.foot = {
        enable = true;
        server.enable = cfg.enableServer;
        settings.main.font = "JetBrainsMono NF:size=10.5";
      };

      systemd.user.services.foot = lib.mkIf cfg.enableServer {
        Service = {
          Environment = [
            "WAYLAND_DISPLAY=${myconfig.wayland.display}"
            "PATH=${
              lib.concatMapStrings (dir: dir + ":") homeconfig.home.sessionPath
            }${homeconfig.home.profileDirectory}/bin"
          ];
        };
      };
    };
}
