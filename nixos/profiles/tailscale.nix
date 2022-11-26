{
  pkgs,
  config,
  ...
}: {
  services.tailscale = {
    enable = true;
  };

  networking.firewall.allowedUDPPorts = [
    config.services.tailscale.port
  ];

  environment.systemPackages = [
    pkgs.tailscale
  ];
}
