{ delib, host, pkgs, ... }:
delib.module {
  name = "dbeaver";

  options = delib.singleEnableOption (host.guiFeatured && host.codingFeatured);

  home.ifEnabled = {
    home.packages = [
      pkgs.dbeaver-bin
    ];
  };
}
