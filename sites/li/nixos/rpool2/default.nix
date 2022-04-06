{
  imports = [
    ./persistence.nix
    ./extras.nix
  ];

  fileSystems."/nix" = {
    device = "rpool2/local/nix";
    fsType = "zfs";
    options = ["noatime"];
  };

  fileSystems."/persist" = {
    device = "rpool2/safe/persist";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "rpool2/safe/home";
    fsType = "zfs";
  };
}
