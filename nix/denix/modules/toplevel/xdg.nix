{
  delib,
  pkgs,
  homeconfig,
  ...
}:
delib.module {
  name = "xdg";

  options = delib.singleEnableOption (!pkgs.stdenv.isDarwin);

  home.ifEnabled = {
    xdg = {
      configHome = "${homeconfig.home.homeDirectory}/.config";
      cacheHome = "${homeconfig.home.homeDirectory}/.cache";
      dataHome = "${homeconfig.home.homeDirectory}/.local/share";
    };
  };

  nixos.ifEnabled = {
    environment.pathsToLink = [
      "/share/applications"
      "/share/xdg-desktop-portal"
    ];
  };
}
