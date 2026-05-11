{ delib, pkgs, ... }:
delib.module {
  name = "rime";

  options =
    with delib;
    moduleOptions {
      enable = boolOption false;
    };

  home.ifEnabled = {
    home.packages = [ pkgs.rime-cli ];

    # Simply adding packages to home.packages didn't work, so explicitly
    # generate a file endpoint.
    xdg.dataFile."rime-data".source = pkgs.symlinkJoin {
      name = "rime-data";
      paths = [
        (pkgs.rime-data + "/share/rime-data")
      ];
    };
  };
}
