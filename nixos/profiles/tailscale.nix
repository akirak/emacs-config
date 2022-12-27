{
  pkgs,
  config,
  ...
}: {
  services.tailscale = {
    enable = true;
  };

  networking.firewall = {
    checkReversePath = "loose";
    trustedInterfaces = [
      "tailscale0"
    ];
    allowedTCPPorts = [
      # SSH
      22
    ];
    allowedUDPPorts = [
      config.services.tailscale.port
    ];
  };

  environment.systemPackages = [
    pkgs.tailscale
  ];
}
