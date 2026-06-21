{
  delib,
  host,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "dictd";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.isDesktop;

      DBs = listOfOption package [ ];

      dictionaries = {
        wordnet = boolOption false;
        # Useful as an alternative to dict.org
        wiktionary = boolOption true;
      };
    };

  nixos.ifEnabled = { cfg, ... }: {
    services.dictd = {
      enable = true;
      DBs =
        cfg.DBs
        ++ (lib.optional cfg.dictionaries.wordnet pkgs.dictdDBs.wordnet)
        ++ (lib.optional cfg.dictionaries.wiktionary pkgs.dictdDBs.wiktionary);
    };
  };
}
