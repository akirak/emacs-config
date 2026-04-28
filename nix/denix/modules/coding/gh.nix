{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "gh";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;
    };

  home.ifEnabled =
    { ... }:
    {
      home.packages = with pkgs; [
        gh
        gh-enhance
      ];
    };
}
