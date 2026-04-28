{ delib, ... }:
delib.module {
  name = "git";

  options =
    with delib;
    moduleOptions {
      enable = boolOption true;
    };

  home.ifEnabled =
    { ... }:
    {
      programs.git = {
        enable = true;
      };
    };
}
