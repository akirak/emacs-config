# Other hardware-specific settings
{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.clinfo ];

  nix.settings.max-jobs = pkgs.lib.mkDefault 6;
  powerManagement.cpuFreqGovernor = pkgs.lib.mkDefault "powersave";

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-compute-runtime
    ];
  };

  hardware.bluetooth = {
    enable = true;
  };
}
