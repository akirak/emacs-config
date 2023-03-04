{homeUser, ...} @ args: {
  users.users.${homeUser} = {
    uid = 1000;
    description = "default";
    isNormalUser = true;

    extraGroups = [
      "wheel"
      "video"
      "audio"
      "disk"
      "networkmanager"
      "systemd-journal"
      "docker"
    ];

    hashedPassword = "$6$3LmgpFGu4WEeoTss$9NQpF4CEO8ivu0uJTlDYXdiB6ZPHBsLXDZr.6S59bBNxmNuhirmcOmHTwhccdgSwq7sJOz2JbOOzmOCivxdak0";
  };
}
