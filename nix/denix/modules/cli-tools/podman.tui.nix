{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "podman.tui";

  options = delib.singleEnableOption host.cliFeatured;

  nixos.ifEnabled = {
    environment.systemPackages = [
      pkgs.podman-tui
    ];
  };
}
