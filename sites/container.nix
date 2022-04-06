{
  hostName = "container";
  username = "akirakomamura";

  homeModules = [
    "base"
  ];

  extraHomeModules = [
    {
      home.enableNixpkgsReleaseCheck = false;
    }
  ];

  nixos = {
    users.users = {
      hashedPassword = "$6$8MSnOKraU6m/OfGz$Bn.wcdaTl/.3hwsMWyEKYyuVea8TCnC2dQtHcI2A15ke9R726wtbTsX06jeatb.Fvu4CiDPbiadkaTRBLUjoB/";
    };
  };
}
