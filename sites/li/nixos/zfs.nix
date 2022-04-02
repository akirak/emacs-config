{
  boot.supportedFilesystems = [ "zfs" ];
  boot.initrd.supportedFilesystems = [ "zfs" ];
  boot.zfs.requestEncryptionCredentials = true;
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoScrub.enable = true;
  # Configure ARC up to 4 GiB
  boot.kernelParams = [ "zfs.zfs_arc_max=4294967296" ];
}
