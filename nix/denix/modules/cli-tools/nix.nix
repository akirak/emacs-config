# Also see nh.nix
{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.nix";

  options = with delib; {
    enable = boolOption host.cliFeatured;
    enableDatabase = boolOption true;
  };

  home.ifEnabled =
    { cfg, myconfig, ... }:
    {
      programs.nix-index = {
        enable = true;
        enableZshIntegration = myconfig.zsh.enable;
        # Assumes the overlay from github:nix-community/nix-index-database is
        # used
        package = if cfg.enableDatabase then pkgs.nix-index-with-db or pkgs.nix-index else pkgs.nix-index;
      };

      programs.nix-your-shell = {
        enable = true;
        enableZshIntegration = myconfig.zsh.enable;
      };

      home.packages = with pkgs; [
        nix-prefetch-git
        nix-output-monitor
        nix-fast-build
        nix-inspect
        cachix
      ];
    };
}
