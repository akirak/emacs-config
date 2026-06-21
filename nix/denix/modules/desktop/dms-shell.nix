{ delib, host, ... }:
delib.module {
  name = "dms-shell";

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
    # Based on https://danklinux.com/docs/dankmaterialshell/nixos
    programs.dms-shell = {
      enable = true;

      systemd = {
        enable = true;
        restartIfChanged = true;
      };

      enableSystemMonitoring = true;
      enableAudioWavelength = true;
      enableCalendarEvents = true;
    };
  };
}
