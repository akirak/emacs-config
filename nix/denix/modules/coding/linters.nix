{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "coding.linters";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      yamllint
      deadnix
    ];
  };
}
