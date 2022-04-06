{pkgs, ...}: {
  services.udev.packages = [
    pkgs.yubikey-personalization
  ];

  services.pcscd = {
    enable = true;
  };
}
