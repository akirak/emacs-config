{pkgs, ...}: {
  home.packages =
    pkgs.lib.attrVals [
      "ripgrep"
      "fd"
      "jq"
      "tealdeer"

      # Nix
      "cachix"
      "nix-prefetch-git"
      "manix"
      "nix-index"

      # System
      "glances"

      # Disk
      "du-dust"
      "duf"

      # Net
      "xh"
      "rclone"
    ]
    pkgs;

  programs.bat.enable = true;

  programs.broot = {
    # Causes a build failure
    enable = false;
    enableZshIntegration = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };
}
