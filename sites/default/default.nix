{
  hostName = "localhost";
  username = "defaultUser";

  homeModules = [
    "desktop"
  ];

  extraHomeModules = [
    {
      home.enableNixpkgsReleaseCheck = false;
    }
  ];

  nixos = {
    users.users = { };
  };
}
