{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "pi";

  options = with delib; moduleOptions {
    enable = boolOption host.codingFeatured;

    package = packageOption pkgs.pi-coding-agent;
  };

  home.ifEnabled = {cfg,...}: {
    home.packages = [
      cfg.package
    ];
  };
}
