{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    libatasmart # For skdump
    smartmontools # For smartctl
  ];

  services.smartd = {
    enable = false;
    notifications = {
      x11 = {
        enable = true;
      };
    };
  };
}
