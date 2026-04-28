{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "sandbox";

  options = delib.singleEnableOption host.codingFeatured;

  nixos.ifEnabled = {
    environment.systemPackages = with pkgs; [
      bubblewrap
    ];
  };
}
