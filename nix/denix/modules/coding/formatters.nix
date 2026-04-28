{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "coding.formatters";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      yamlfmt
    ];
  };
}
