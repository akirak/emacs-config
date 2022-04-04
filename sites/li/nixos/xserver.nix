{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    # videoDrivers = [
    #   "modesetting"
    # ];
    # useGlamor = true;
    xrandrHeads = [
      {
        output = "DP-1";
        monitorConfig = ''
          Option "Primary" "true"
          Option "Mode" "2560x1440"
          Option "Position" "1920 0"
        '';
      }
      {
        output = "HDMI2";
        monitorConfig = ''
          Option "Mode" "1920x1080"
          Option "Position" "0 0"
        '';
      }
    ];
  };
}
