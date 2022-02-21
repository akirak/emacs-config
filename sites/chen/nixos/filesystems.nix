# Configuration for non-ZFS file systems on the system SSD
{
  fileSystems."/" =
    {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "defaults" "size=10G" "mode=755" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/4436-C0E5";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/6e1f6320-2dd3-45ad-8d83-e916dffc9f1d"; }];
}
