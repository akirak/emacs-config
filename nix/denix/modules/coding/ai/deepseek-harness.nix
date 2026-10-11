{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "deepseek-harness";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.ai-tools.dsh
    ];
  };
}
