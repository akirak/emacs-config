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
      substituters = ["https://nrdxp.cachix.org"];
      trusted-public-keys = ["nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4="];
    }
    {
      substituters = ["https://hydra.iohk.io"];
      trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    }
    {
      substituters = ["https://akirak.cachix.org"];
      trusted-public-keys = ["akirak.cachix.org-1:WJrEMdV1dYyALkOdp/kAECVZ6nAODY5URN05ITFHC+M="];
    }
  ]
