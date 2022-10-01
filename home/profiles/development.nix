{pkgs, ...}: {
  imports = [
    ./git
  ];

  home.packages = with pkgs; [
    gh
    pre-commit
    nixpkgs-fmt
    alejandra
    deadnix
    squasher
    drawio

    hunspellDicts.en_US
  ];

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
