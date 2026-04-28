{
  delib,
  lib,
  host,
  ...
}:
delib.module {
  name = "xdg.mime";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;
      defaultApplications = attrsOption { };
    };

  home.ifEnabled =
    { cfg, ... }:
    let
      defaultApplications = lib.filterAttrs (_: value: value != null) cfg.defaultApplications;
    in
    {
      xdg = {
        mime.enable = true;
        mimeApps = {
          enable = true;
          inherit defaultApplications;
          associations.added = defaultApplications;
        };
      };
    };
}
