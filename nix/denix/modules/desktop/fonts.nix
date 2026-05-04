{
  delib,
  pkgs,
  host,
  ...
}:
let
  fontModule =
    with delib;
    submoduleOption {
      options = {
        package = noDefault (packageOption null);
        name = noDefault (strOption null);
      };
    };
in
delib.module {
  name = "fonts";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;

      monospace = fontModule {
        package = pkgs.nerd-fonts.jetbrains-mono;
        name = "JetBrainsMono Nerd Font";
      };

      sansSerif = fontModule {
        package = pkgs.lato;
        name = "Lato";
      };

      serif = fontModule {
        package = pkgs.merriweather;
        name = "Merriweather";
      };
    };

  nixos.ifEnabled =
    { cfg, ... }:
    {
      fonts = {
        # International fonts are provided from the localization module.
        packages = [
          cfg.monospace.package
          cfg.sansSerif.package
          cfg.serif.package
        ];

        fontconfig.defaultFonts = {
          monospace = [ cfg.monospace.name ];
          sansSerif = [ cfg.sansSerif.name ];
          serif = [ cfg.serif.name ];
        };
      };
    };

  # Not adding fonts for home-manager. Some platforms (e.g. macOS) may ship with
  # propriety fonts that look better than the free ones.
}
