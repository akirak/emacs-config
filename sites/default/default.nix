{
  hostName = "localhost";
  username = "defaultUser";

  homeModules = [
    "desktop"
  ];

  extraHomeModules = [
    {
      # You need to disable this check if the versions mismatch
      home.enableNixpkgsReleaseCheck = false;
    }
  ];

  nixos = {
    users.users = {
      # You need to set `users.mutableUsers = true` to update the password later
      # initialHashedPassword = "";

      # Use `mkpasswd -m sha-512`
      # hashedPassword = "";
    };
  };
}
