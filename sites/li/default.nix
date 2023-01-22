let
  username = "akirakomamura";
in {
  hostName = "li";
  inherit username;

  homeModules = [
    "river"
    "symlinks"
    "personal"
    # "work"
  ];

  extraHomeModules = [
    ({
      pkgs,
      site,
      ...
    }: {
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
    })
  ];

  nixos = {
    users.users = {
      hashedPassword = "$6$3LmgpFGu4WEeoTss$9NQpF4CEO8ivu0uJTlDYXdiB6ZPHBsLXDZr.6S59bBNxmNuhirmcOmHTwhccdgSwq7sJOz2JbOOzmOCivxdak0";
    };
  };
}
