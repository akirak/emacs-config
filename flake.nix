# Copyright (C) 2021-2026 Akira Komamura
# SPDX-License-Identifier: MIT

{
  inputs = {
    nixpkgs.follows = "flake-pins/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    # Use a pinned version of Emacs executable
    flake-pins.url = "github:akirak/flake-pins";

    twist.url = "github:emacs-twist/twist.nix";
    org-babel.url = "github:emacs-twist/org-babel";

    tree-sitter-astro = {
      url = "github:virchau13/tree-sitter-astro";
      flake = false;
    };

    lsp-proxy.url = "github:jadestrong/lsp-proxy";
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
      flake-parts,
      nixpkgs,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } (
      top@{
        flake-parts-lib,
        self,
        inputs,
        ...
      }:
      let
        inherit (nixpkgs) lib;

        recipesDir = ./recipes;
        lockDir = ./lock;

        # Define the configuration revision based on the repository's Git revision.
        configurationRevision = import ./nix/lib/configurationRevision.nix self;

        makeEmacsEnvWithPkgs =
          pkgs:
          import ./nix/twist/make-env.nix {
            orgInitSource = ./emacs-config.org;
            inputs =
              inputs
              // top.config.partitions.packages.extraInputs
              // {
                inherit self;
              };
            inherit
              lib
              pkgs
              recipesDir
              lockDir
              configurationRevision
              ;
          };

        overlay = lib.composeManyExtensions [
          inputs.org-babel.overlays.default

          inputs.flake-pins.overlays.default

          (
            final: prev:
            let
              inherit (prev.stdenv.hostPlatform) system;
            in
            {
              lsp-proxy = inputs.lsp-proxy.packages.${system}.default;
              playwright-mcp = inputs.playwright-mcp.packages.${system}.default;

              emacs-git-pgtk = inputs.flake-pins.packages.${system}.emacs-pgtk;
              emacs-git = inputs.flake-pins.packages.${system}.emacs;

              emacs-env = makeEmacsEnvWithPkgs final {
                emacsPackage = inputs.flake-pins.packages.${system}.emacs-pgtk;
              };
            }
          )

          # Merge these overlays if necessary.

          # makeEmacsTwistArchive
          # inputs.archiver.overlays.default

          # emacsTwist2Elpa
          # inputs.twist2elpa.overlays.default
        ];
      in
      {
        # Explicitly define supported systems
        systems = [
          "x86_64-linux"
          # "aarch64-linux"
          # "aarch64-darwin"
        ];

        imports = [
          # Support partitions
          flake-parts.flakeModules.partitions
        ];

        flake = {
          overlays.default = overlay;

          lib = {
            denixModules = ./nix/denix/modules;
            denixInputs = top.config.partitions.configs.extraInputs // inputs;
          };

          checks.x86_64-linux.homeConfiguration =
            self.homeConfigurations."akirakomamura@default-x86_64-linux".activationPackage;
        };

        perSystem =
          { system, ... }:
          {
            checks = {
              # Check if the elisp packages are successfully built.
              elisp-packages = self.packages.${system}.emacs-config.overrideScope (
                _tself: _tsuper: {
                  executablePackages = [ ];
                }
              );

              # depsCheck = fullEmacsEnv.depsCheck;
            };
          };

        partitionedAttrs = {
          devShells = "shells";
          formatter = "shells";
          packages = "packages";
          apps = "packages";
          checks = "packages";
          overlays = "configs";
          homeConfigurations = "configs";
        };

        partitions = {
          shells = {
            extraInputsFlake = ./nix/flake-parts/partitions/shells;
            module = {
              imports = [
                ./nix/flake-parts/partitions/shells/flake-module.nix
              ];
            };
          };
          configs = {
            extraInputsFlake = ./nix/flake-parts/partitions/configs;
            module = (
              flake-parts-lib.importApply ./nix/flake-parts/partitions/configs/flake-module.nix {
                inherit overlay;
                paths = [
                  ./nix/denix
                ];
              }
            );
          };
          packages = {
            extraInputsFlake = ./nix/flake-parts/partitions/packages;
            module = {
              perSystem =
                {
                  system,
                  pkgs,
                  lib,
                  ...
                }:
                let
                  emacs-pgtk = inputs.flake-pins.packages.${system}.emacs-pgtk;

                  emacs-nonpgtk = inputs.flake-pins.packages.${system}.emacs;

                  runInTmpdir = pkgs.callPackage ./nix/lib/tmpInitDirWrapper.nix { } "emacs-tmpdir";
                in
                {
                  _module.args.pkgs = import inputs.nixpkgs {
                    inherit system;
                    overlays = [
                      overlay
                    ];
                    config.allowUnfreePredicate =
                      pkg:
                      builtins.elem (lib.getName pkg) [
                        "copilot-language-server"
                      ];
                  };

                  packages = {

                    emacs-config = pkgs.emacs-env;

                    emacsclient = pkgs.callPackage ./nix/lib/emacsclient.nix {
                      emacs = emacs-pgtk;
                    };

                    # An extra utility for updating elisp packages by one commit per
                    # author.
                    update-elisp-lock = pkgs.callPackage ./nix/lib/updateElispLock.nix {
                      lockDirName = "lock";
                    };

                    update-ai-models = pkgs.callPackage ./nix/lib/writeAiModels.nix { };

                    emacs-pgtk-on-tmpdir = runInTmpdir (
                      pkgs.emacs-env.override {
                        emacsPackage = emacs-pgtk;
                      }
                    );

                    emacs-on-tmpdir = runInTmpdir (
                      pkgs.emacs-env.override {
                        emacsPackage = emacs-nonpgtk;
                      }
                    );

                    # Using Emacs stable for reproducing configuration issues.
                    emacs-stable-on-tmpdir = runInTmpdir (
                      pkgs.emacs-env.override {
                        emacsPackage = pkgs.emacs;
                      }
                    );
                  };

                  apps = pkgs.emacs-env.makeApps { lockDirName = "lock"; };
                };
            };
          };
        };
      }
    );
}
