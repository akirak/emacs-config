{
  delib,
  host,
  pkgs,
  ...
}:
delib.module {
  name = "do-not-track";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;
    };

  home.ifEnabled = {
    home.sessionVariables = {
      "DO_NOT_TRACK" = "1";
    };

    programs.zsh.sessionVariables = {
      "DO_NOT_TRACK" = "1";
    };
  };
}
