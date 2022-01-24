{
  hostName = "chen";
  username = "akirakomamura";

  homeModules = [
    "xmonad"
    "symlinks"
    "personal"
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
