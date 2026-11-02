{ delib, pkgs, ... }:
delib.module {
  name = "h5i";

  options = delib.singleEnableOption false;

  home.ifEnabled = {
    home.packages = [
      pkgs.customPackages.h5i
    ];
  };
}
