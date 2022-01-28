{ self, pkgs, ... }:
{
  nix.generateRegistryFromInputs = true;
  nix.generateNixPathFromInputs = true;
  # nix.linkInputs = true;

  # Let 'nixos-version --json' know about the Git revision of this
  # flake.
  system.configurationRevision = pkgs.lib.mkIf (self ? rev) self.rev;

  environment.sessionVariables = {
    "TMPDIR" = "/tmp";
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Asia/Tokyo";
  services.xserver.layout = "us";
}
