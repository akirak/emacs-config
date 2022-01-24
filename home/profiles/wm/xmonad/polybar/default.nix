{ pkgs, ... }:
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      githubSupport = true;
      pulseSupport = true;
      iwSupport = true;
    };
    extraConfig = ''
      [module/xmonad]
      type = custom/script
      exec = ${pkgs.xmonad-log}/bin/xmonad-log
      tail = true
    '';
    config = ./polybar.ini;
    script = ''
      polybar top &
      polybar bottom &
    '';
  };
}
