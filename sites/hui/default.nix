let
  username = "akirakomamura";
in {
  hostName = "hui";
  inherit username;

  # Turn on wireless card. If it works, move this later to the NixOS module
  hardware.enableRedistributableFirmware = true;

  homeModules = [
    "symlinks"
    "personal"
  ];

  extraHomeModules = [
    ({...}: {
      programs.chromium.enable = false;
    })
  ];

  nixos = {
    users.users = {
      hashedPassword = "$6$3LmgpFGu4WEeoTss$9NQpF4CEO8ivu0uJTlDYXdiB6ZPHBsLXDZr.6S59bBNxmNuhirmcOmHTwhccdgSwq7sJOz2JbOOzmOCivxdak0";
    };
  };
}
