{
  imports = [
    ../display-managers/gdm.nix
  ];

  services.xserver.desktopManager.gnome = {
    enable = true;
  };

  services.gnome = {
    chrome-gnome-shell.enable = true;
  };
}
