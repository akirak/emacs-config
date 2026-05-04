{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "codex";

  options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.ai-tools.codex
    ];

    home.file.".codex/AGENTS.md".source = ./etc/AGENTS.md;
  };
}
