{
  config,
  pkgs,
  ...
}: {
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  environment.systemPackages = [
    pkgs.lshw
    pkgs.git
  ];

  boot.initrd.kernelModules = [
    "usbcore"
    "nvme"
    "sdhci_pci"
    "mmc_block"
    "xhci_hcd"
    "usb-storage"
  ];

  boot.initrd.luks.devices = {
    cryptroot = {
      allowDiscards = true;
    };
    # cryptdata = {
    #   allowDiscards = true;
    #   preLVM = true;
    # };
  };

  boot.supportedFilesystems = ["btrfs"];
  boot.initrd.supportedFilesystems = ["btrfs"];

  # fileSystems."/home" = {
  #   neededForBoot = false;
  # };

  boot.runSize = "64m";
  boot.devSize = "256m";
  boot.devShmSize = "256m";
}
