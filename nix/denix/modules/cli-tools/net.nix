{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "cli-tools.net";

  options = delib.singleEnableOption host.cliFeatured;

  home.ifEnabled = {
    home.packages = with pkgs; [
      xh
      dig
      nmap
      tcpdump
      nssTools
    ];
  };
}
