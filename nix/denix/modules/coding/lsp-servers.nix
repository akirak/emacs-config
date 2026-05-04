{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "coding.lsp-servers";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      vscode-langservers-extracted # Primarily for the JSON server
      tix
      just-lsp
    ];
  };
}
