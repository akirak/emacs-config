{
  delib,
  host,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "emacs";

  options =
    with delib;
    moduleOptions {
      installFonts = boolOption host.guiFeatured;
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.packages = lib.optionals cfg.installFonts (
        with pkgs;
        [
          # Font families used in my Emacs config
          cascadia-code
          inter
          source-han-sans
          noto-fonts-color-emoji
          symbola
        ]
      );
    };
}
