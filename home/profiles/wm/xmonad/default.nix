{pkgs, ...}: {
  imports = [
    ./lock.nix
    ./dunst.nix
    ./picom.nix
    ./polybar
  ];

  home.packages = with pkgs; [
    arandr
    pavucontrol
    pasystray
    pamixer # for actions
    dunst # for dunstctl
    xmonad-log
    rofi-systemd
  ];

  xsession = {
    enable = true;

    initExtra = ''
      if [ -f ~/.screenlayout/default.sh ]; then
        . ~/.screenlayout/default.sh
      fi
      setxkbmap -option ctrl:nocaps
      pasystray &
      blueman-applet &
      nm-applet --sm-disable --indicator &
    '';

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = h: [
        h.dbus
      ];
      config = ./xmonad.hs;
      libFiles = {
        "Polybar.hs" = ./lib/Polybar.hs;
        "Actions.hs" = ./lib/Actions.hs;
        "Queries.hs" = ./lib/Queries.hs;
      };
    };
  };

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };
}
