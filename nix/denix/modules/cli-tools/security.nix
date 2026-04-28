{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.security";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      openssl
    ];
  };
}
