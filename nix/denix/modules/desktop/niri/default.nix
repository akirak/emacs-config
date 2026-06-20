{ delib, pkgs, ... }:
delib.module {
  name = "niri";

  options.niri = with delib; {
    enable = boolOption false;
  };

  nixos.ifEnabled =
    {
      environment.sessionVariables.NIXOS_OZONE_WL = "1";

      programs.niri = {
        enable = true;
      };

      environment.systemPackages = [
        # Enables XWayland. See https://niri-wm.github.io/niri/Xwayland.html
        pkgs.xwayland-satellite
      ];

      environment.etc."wayland-sessions/Niri.desktop".text = ''
        [Desktop Entry]
        Name=Niri
        Exec=niri-session
        Type=Application
      '';
    };

  home.ifEnabled = {
    xdg.configFile."niri/config.kdl".source = ./etc/config.kdl;

    # Requires the home module from the xremap flake
    services.xremap.withNiri = true;
  };
}
