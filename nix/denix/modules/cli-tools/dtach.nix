{ delib, pkgs, host, ... }:
delib.module {
  name = "dtach";

  options = delib.singleEnableOption host.isDesktop;

  home.ifEnabled = {
    home.packages = [
      pkgs.dtach
    ];
  };
}
