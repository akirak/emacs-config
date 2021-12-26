{
  nix.generateRegistryFromInputs = true;
  nix.generateNixPathFromInputs = true;
  # nix.linkInputs = true;

  # Let 'nixos-version --json' know about the Git revision of this
  # flake.
  system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
}
