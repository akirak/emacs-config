{pkgs, ...}: {
  home.packages = [
    pkgs.kanshi
  ];

  xdg.configFile."kanshi/config".text = ''
    profile {
      output "Unknown VA32AQ K3LMAS000141 (HDMI-A-2)" mode 2560x1440 position 0,0
      output "Dell Inc. DELL S2421HS CBPT223 (DP-1)" mode 1920x1080 position 2560,100
    }
  '';
}
