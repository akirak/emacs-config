{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "openshell";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.openshell
    ];
  };
}
