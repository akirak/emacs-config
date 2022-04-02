# Configuration for non-ZFS file systems on the system SSD
{
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/8d814cba-6716-4951-94b8-331025c318f2";
      preLVM = true;
    };
  };

  fileSystems."/" =
    {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "defaults" "size=10G" "mode=755" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/89D8-EFFA";
      fsType = "vfat";
    };


  swapDevices = [ ];
}
