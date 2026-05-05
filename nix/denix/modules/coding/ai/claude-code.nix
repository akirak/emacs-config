{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "claude-code";

  # I am currently not using Claude Code, so this should be disabled
  # unconditionally. If you ever use claude, you could enable it explicitly on
  # individual hosts.
  options = delib.singleEnableOption false;

  home.ifEnabled = {
    home.packages = [
      pkgs.ai-tools.claude-code
    ];
  };
}
