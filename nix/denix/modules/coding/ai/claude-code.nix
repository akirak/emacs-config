{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "claude-code";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;
    };

  home.ifEnabled = {
    home.packages = [
      pkgs.claude-code
    ];
  };
}
