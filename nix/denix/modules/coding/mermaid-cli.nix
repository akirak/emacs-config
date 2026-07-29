{
  delib,
  pkgs,
  ...
}:
delib.module {
  name = "mermaid-cli";

  options = delib.singleEnableOption false;

  home.ifEnabled = {
    home.packages = [
      pkgs.mermaid-cli
    ];
  };
}
