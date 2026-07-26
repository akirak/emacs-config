{
  delib,
  host,
  pkgs,
  ...
}:
delib.module {
  name = "playwright-cli";

  options = delib.singleEnableOption host.guiFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.playwright-cli
    ];
  };
}
