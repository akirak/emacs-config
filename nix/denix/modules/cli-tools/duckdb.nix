{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.duckdb";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      duckdb
    ];

    home.file.".duckdbrc".text = ''
      .highlight_mode light
    '';
  };
}
