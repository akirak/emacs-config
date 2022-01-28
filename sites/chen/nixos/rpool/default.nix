{
  imports = [
    ./persistence.nix
    ./extras.nix
  ];

  fileSystems."/nix" =
    {
      device = "rpool/local/nix";
      fsType = "zfs";
      options = [ "noatime" ];
    };

  fileSystems."/persist" =
    {
      device = "rpool/safe/persist";
      fsType = "zfs";
      neededForBoot = true;
    };

  fileSystems."/home" =
    {
      device = "rpool/safe/home";
      fsType = "zfs";
    };

}
