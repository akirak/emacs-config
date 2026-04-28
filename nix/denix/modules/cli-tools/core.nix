{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.core";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      ripgrep
      fd
      jq
    ];

    programs = {
      bat.enable = true;
      eza.enable = true;
    };
  };
}
