{
  config,
  pkgs,
  ...
}: {
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelModules = ["iwlwifi" "acpi_call"];
  boot.extraModulePackages = with config.boot.kernelPackages; [
    acpi_call
  ];

  hardware.firmware = [pkgs.wireless-regdb];

  environment.systemPackages = [
    pkgs.lshw
    pkgs.git
  ];
  hardware.enableRedistributableFirmware = true;

  boot.initrd.kernelModules = ["usbcore" "nvme" "sdhci_pci" "mmc_block" "xhci_hcd" "usb-storage"];

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
