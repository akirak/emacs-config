# Copyright (C) 2021-2025 Akira Komamura
# SPDX-License-Identifier: MIT

{
  inputs = {
    # Should be updated from flake-pins: <https://github.com/akirak/flake-pins>
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";

    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-filter.url = "github:numtide/nix-filter";

    # Use a pinned version of Emacs executable
    flake-pins.url = "github:akirak/flake-pins";

    # Emacs Twist
    org-babel.url = "github:emacs-twist/org-babel";
    twist.url = "github:emacs-twist/twist.nix";
    twist-overrides.url = "github:emacs-twist/overrides";

    # Package registries for Twist
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };
    gnu-elpa = {
      # Use a GitHub mirror for a higher availability
      url = "github:elpa-mirrors/elpa";
      # url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    nongnu-elpa = {
      # Use a GitHub mirror for a higher availability
      url = "github:elpa-mirrors/nongnu";
      # url = "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
      flake = false;
    };
    gnu-elpa-archive = {
      url = "file+https://elpa.gnu.org/packages/archive-contents";
      flake = false;
    };
    nongnu-elpa-archive = {
      url = "file+https://elpa.nongnu.org/nongnu/archive-contents";
      flake = false;
    };

    # I do want to move the below inputs into a flake partition, but it looks
    # impossible to handle the follows dependencies.

    # Libraries for providing alternative formats of the config.
    twist2elpa = {
      url = "github:emacs-twist/twist2elpa";
      inputs.twist.follows = "twist";
    };
    archiver.url = "github:emacs-twist/twist-archiver";

    tree-sitter-astro = {
      url = "github:virchau13/tree-sitter-astro";
      flake = false;
    };

    playwright-mcp.url = "github:akirak/nix-playwright-mcp";
  };

  nixConfig = {
    extra-substituters = [
      "https://akirak.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "akirak.cachix.org-1:WJrEMdV1dYyALkOdp/kAECVZ6nAODY5URN05ITFHC+M="
    ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-parts,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.partitions
        ./flake-module.nix
        ./modules/utils-flake-module.nix
      ];

      systems = import inputs.systems;

      partitions = {
        checks = {
          extraInputsFlake = ./partitions/checks;
          module = {
            imports = [ ./partitions/checks/flake-module.nix ];
          };
        };
      };

      partitionedAttrs = {
        devShells = "checks";
      };
    };
}
