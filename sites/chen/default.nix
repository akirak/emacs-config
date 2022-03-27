{
  hostName = "chen";
  username = "akirakomamura";

  homeModules = [
    "xmonad"
    "symlinks"
    "personal"
  ];

  extraHomeModules = [
    ({ pkgs, ... }: {
      home.enableNixpkgsReleaseCheck = false;
      programs.emacs-config-old.enable = true;

      programs.emacs-unsafe = {
        extraFeatures = [
          "beancount"
        ];

        extraDirsToTryBind = [
          "/git-annex"
          "/assets"
        ];
      };

      home.packages = [
        pkgs.wine
      ];
    })
  ];

  nixos = {
    users.users = {
      hashedPassword = "$6$3LmgpFGu4WEeoTss$9NQpF4CEO8ivu0uJTlDYXdiB6ZPHBsLXDZr.6S59bBNxmNuhirmcOmHTwhccdgSwq7sJOz2JbOOzmOCivxdak0";
    };
  };
}
