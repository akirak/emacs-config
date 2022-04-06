{
  description = "Flake for Firefox";

  inputs.flake-utils = {
    url = "github:numtide/flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.mozilla.url = "github:mozilla/nixpkgs-mozilla";

  outputs = {
    nixpkgs,
    flake-utils,
    mozilla,
    ...
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          mozilla.overlay
        ];
      };
    in rec {
      packages = flake-utils.lib.flattenTree {
        inherit (pkgs.latest) firefox-beta-bin;
      };
      defaultPackage = pkgs.latest.firefox-beta-bin;
    });
}
