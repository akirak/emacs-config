{pkgs, ...}: {
  imports = [
    ./profiles/non-desktop-environment.nix
  ];

  environment.systemPackages = [
    pkgs.xdg-utils
  ];

  # no display manager

  xdg.portal.wlr = {
    enable = true;
  };

  security.pam.services.swaylock = {};
}
