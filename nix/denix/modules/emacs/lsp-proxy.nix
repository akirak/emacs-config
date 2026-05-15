{
  delib,
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
        # Currently not using lsp-proxy, so disable this unconditionally.
        enable = boolOption false;
        # enable = boolOption myconfig.emacs.enable;

        configPath = readOnly (
          strOption (homeconfig.programs.emacs-twist.directory + "/lsp-proxy/languages.json")
        );
      };
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.file.${cfg.configPath}.source =
        homeconfig.programs.emacs-twist.config.callPackage ../../../lsp-proxy-config.nix
          { };
    };
}
