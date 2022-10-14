{lib, ...}: let
  makeBinaryCacheSettings = entries:
    lib.pipe entries [
      (lib.zipAttrsWithNames ["substituters" "trusted-public-keys"] (_: vs: vs))
      (lib.mapAttrs (_: vs: builtins.concatLists vs))
      (settings: {nix = {inherit settings;};})
    ];
in
  makeBinaryCacheSettings
  [
    {
      substituters = ["https://cache.nixos.org/"];
    }
    {
      substituters = ["https://nix-community.cachix.org"];
      trusted-public-keys = ["nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="];
    }
    {
      substituters = ["https://akirak.cachix.org"];
      trusted-public-keys = ["akirak.cachix.org-1:WJrEMdV1dYyALkOdp/kAECVZ6nAODY5URN05ITFHC+M="];
    }
  ]
