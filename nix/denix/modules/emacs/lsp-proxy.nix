{
  delib,
  pkgs,
  homeconfig,
  ...
}:
delib.module {
  name = "emacs.lsp-proxy";

  options =
    { myconfig, ... }:
    with delib;
    {
      emacs.lsp-proxy = {
        enable = boolOption myconfig.emacs.enable;

        configPath = readOnly (
          strOption (homeconfig.programs.emacs-twist.directory + "/lsp-proxy/languages.toml")
        );
      };
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.file.${cfg.configPath}.source = pkgs.callPackage ../../../lsp-proxy-config.nix { };
    };
}
