{ delib, host, pkgs, ... }:
delib.module {
  name = "nix-fast-build";

  options = with delib; moduleOptions {
    enable = boolOption host.codingFeatured;
  };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      pkgs.nix-fast-build
    ];
  };
}
