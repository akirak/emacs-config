{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "claude-code";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.ai-tools.claude-code
    ];
  };
}
