{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.system";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      btop
      dua
      duf
    ];
  };
}
