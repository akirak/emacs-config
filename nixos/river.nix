{pkgs, ...}: {
  imports = [
    ./profiles/non-desktop-environment.nix
  ];

  environment.systemPackages = [
    pkgs.xdg-utils
  ];

  xdg.portal.wlr = {
    enable = true;
  };

  security.pam.services.swaylock = {};

  services.greetd = {
    enable = true;
    settings.default_session.command = ''
      ${pkgs.greetd.tuigreet}/bin/tuigreet -c river-session
    '';
  };
}
