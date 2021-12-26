{
  users.users.akirakomamura = {
    uid = 1000;
    description = "default";
    isNormalUser = true;

    hashedPassword =
      "$6$8MSnOKraU6m/OfGz$Bn.wcdaTl/.3hwsMWyEKYyuVea8TCnC2dQtHcI2A15ke9R726wtbTsX06jeatb.Fvu4CiDPbiadkaTRBLUjoB/";
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.akirakomamura = { pkgs, ... }: {
      home.packages = pkgs.lib.attrVals [
        "zsh"
        "emacs"
      ]
        pkgs;
    };
  };
}
