# Also see nh.nix
{
  delib,
  pkgs,
  homeconfig,
  host,
  ...
}:
delib.module {
  name = "cli-tools.nix";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled =
    { myconfig, ... }:
    {
      programs.nix-index = {
        enable = true;
        enableZshIntegration = myconfig.zsh.enable;
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
