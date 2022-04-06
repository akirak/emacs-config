# Other hardware-specific settings
{
  pkgs,
  lib,
  config,
  ...
}: {
  # environment.systemPackages = [ pkgs.clinfo ];

  # nix.maxJobs = pkgs.lib.mkDefault 6;
  # powerManagement.cpuFreqGovernor = pkgs.lib.mkDefault "powersave";

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  nixpkgs.config.pafckageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      # vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  # hardware.opengl = {
  #   enable = true;
  #   extraPackages = with pkgs; [
  #     intel-media-driver
  #     intel-compute-runtime
  #   ];
  # };

  # hardware.bluetooth = {
  #   enable = true;
  # };
}
