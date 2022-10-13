{pkgs, ...}: {
  boot.kernelPackages = pkgs.linuxPackages_5_19;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
  # This kernel module is needed if and only if unlock LUKS devices on boot
  # boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  boot.runSize = "64m";
  boot.devSize = "256m";
  boot.devShmSize = "256m";
}
