{lib, ...}: let
  makeBinaryCacheSettings = entries:
    lib.pipe entries [
      (lib.zipAttrsWithNames ["binaryCaches" "binaryCachePublicKeys"] (_: vs: vs))
      (lib.mapAttrs (_: vs: builtins.concatLists vs))
      (nix: {inherit nix;})
    ];
in
  makeBinaryCacheSettings
  [
    {
      binaryCaches = [
        "https://cache.nixos.org/"
      ];
    }
    {
      binaryCaches = ["https://nix-community.cachix.org"];
      binaryCachePublicKeys = ["nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="];
    }
    {
      binaryCaches = ["https://nrdxp.cachix.org"];
      binaryCachePublicKeys = ["nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4="];
    }
    {
      binaryCaches = ["https://hydra.iohk.io"];
      binaryCachePublicKeys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    }
    {
      binaryCaches = ["https://akirak.cachix.org"];
      binaryCachePublicKeys = ["akirak.cachix.org-1:WJrEMdV1dYyALkOdp/kAECVZ6nAODY5URN05ITFHC+M="];
    }
  ]
