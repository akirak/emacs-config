{ delib, homeconfig, ... }:
delib.module {
  name = "emacs";

  options =
    with delib;
    moduleOptions {
      aiModelListPath = readOnly (
        strOption (homeconfig.programs.emacs-twist.directory + "/ai-model-list.txt")
      );
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.file.${cfg.aiModelListPath}.source = ../../../../ai-model-list.txt;
    };
}
