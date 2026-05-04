{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "pi";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.ai-tools.pi
    ];
  };
}
