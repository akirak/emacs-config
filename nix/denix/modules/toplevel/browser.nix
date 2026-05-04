{ delib, host, ... }:
delib.module {
  name = "browser";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;
      desktopFile = allowNull (strOption null);
      enablePsd = boolOption false;
    };

  myconfig.ifEnabled =
    { cfg, ... }:
    {
      xdg.mime = {
        enable = true;
        defaultApplications = {
          "text/html" = cfg.desktopFile;
          "x-scheme-handler/http" = cfg.desktopFile;
          "x-scheme-handler/https" = cfg.desktopFile;
          "image/svg+xml" = cfg.desktopFile;
        };
      };
    };
}
