{
  hostName = "li";
  username = "akirakomamura";

  homeModules = [
    "river"
    "symlinks"
    "personal"
  ];

  extraHomeModules = [
    ({pkgs, ...}: {
      home.enableNixpkgsReleaseCheck = false;

      programs.chromium = {
        enable = true;
        extensions = [
          {
            # Google Input Tools
            id = "mclkkofklkfljcocdinagocijmpgbhab";
          }
        ];
      };

      home.packages = [
        pkgs.wine
        pkgs.tenacity
      ];

      home.stateVersion = "22.05";
    })
  ];

  nixos = {
    users.users = {
      hashedPassword = "$6$3LmgpFGu4WEeoTss$9NQpF4CEO8ivu0uJTlDYXdiB6ZPHBsLXDZr.6S59bBNxmNuhirmcOmHTwhccdgSwq7sJOz2JbOOzmOCivxdak0";
    };
  };
}
