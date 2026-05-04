{
  delib,
  host,
  ...
}:
delib.module {
  name = "direnv";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;
    };

  home.ifEnabled =
    { myconfig, ... }:
    {
      programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        enableZshIntegration = myconfig.zsh.enable;
      };
    };
}
