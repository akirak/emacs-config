{
  imports = [
    ./profiles/utils.nix
    ./profiles/cachix.nix
    ./profiles/starship.nix
  ];

  # It may be better to follow instructions from
  # https://christine.website/blog/paranoid-nixos-2021-07-18
  # 
  # services.tailscale.enable = true;
  # networking.firewall.trustedInterfaces = [ "tailscale0" ];
}
