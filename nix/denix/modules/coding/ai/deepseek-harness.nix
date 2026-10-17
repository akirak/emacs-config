{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "deepseek-harness";

  # Not using for now.
  options = delib.singleEnableOption false;

  # currently available from llm-agents.nix

  # options = delib.singleEnableOption host.codingFeatured;

  home.ifEnabled = {
    home.packages = [
      pkgs.ai-tools.dsh
    ];
  };
}
