{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.development";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      difftastic
      duckdb
      hyperfine
      just
    ];
  };
}
