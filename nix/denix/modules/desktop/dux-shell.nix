{ delib, pkgs, host, ... }:
delib.module {
  name = "dux-shell";

  options = with delib; singleEnableOption host.guiFeatured;

  myconfig.ifEnabled = {
    niri.include = [
      ''
        include "dms/alttab.kdl"
        include "dms/colors.kdl"
        include "dms/layout.kdl"
        include "dms/wpblur.kdl"
      ''
    ];
  };

  nixos.ifEnabled = {
    # Based on https://duxlabs.dev/docs/dms/nixos
    programs.dux-shell = {
      enable = true;

      systemd = {
        enable = true;
        restartIfChanged = true;
      };

      plugins = {
        clipboardManager = {
          enable = false;
        };
      };
    };
  };

  home.ifEnabled = {
    # NOTE: Plugins need to be manually enabled in the Settings of Dux.

    # Non-standard in nixpkgs; Add
    # https://github.com/rochacbruno/DankNiriWindows to pkgs
    xdg.configFile."DankMaterialShell/plugins/NiriWindows".source = pkgs.dux-plugins.niri-windows;
  };
}
