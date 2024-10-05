{
  inputs = {
    # Should be updated from flake-pins: <https://github.com/akirak/flake-pins>
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-filter.url = "github:numtide/nix-filter";

    flake-pins = {
      url = "github:akirak/flake-pins";
      flake = false;
    };
    flake-pins-pkgs = {
      url = "github:akirak/flake-pins?dir=pkgs/cli-tools";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Emacs
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    org-babel.url = "github:emacs-twist/org-babel";
    twist.url = "github:emacs-twist/twist.nix";
    twist-overrides.url = "github:emacs-twist/overrides";
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
    emacs-config-features.url = "github:akirak/emacs-config/develop?dir=presets/default";

    twist2elpa = {
      url = "github:emacs-twist/twist2elpa";
      inputs.twist.follows = "twist";
    };
    archiver.url = "github:emacs-twist/twist-archiver";

    # pre-commit
    flake-no-path = {
      url = "github:akirak/flake-no-path";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };

    tree-sitter-astro = {
      url = "github:virchau13/tree-sitter-astro";
      flake = false;
    };
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
      systems = [ "x86_64-linux" ];

      flake = {
        homeModules.twist = {
          imports = [
            inputs.twist.homeModules.emacs-twist
            (import ./emacs/home-module.nix self.packages)
          ];
        };
      };

      perSystem =
        {
          config,
          system,
          pkgs,
          ...
        }:
        let
          inherit (pkgs) lib emacs-config;
          inherit (builtins) substring;
          profiles = import ./emacs/profiles.nix {
            inherit lib;
            defaultFeatures = import inputs.emacs-config-features;
          } emacs-config;
        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              # Add `emacs-config` to the package.
              (import ./emacs/overlay.nix {
                inherit inputs;
                configurationRevision = "${substring 0 8 self.lastModifiedDate}.${
                  if self ? rev then substring 0 7 self.rev else "dirty"
                }";
              })
              # makeEmacsTwistArchive
              inputs.archiver.overlays.default
              # emacsTwist2Elpa
              inputs.twist2elpa.overlays.default
              # Bring custom packages into the scope for native dependencies.
              (_: _: {
                flake-no-path = inputs.flake-no-path.defaultPackage.${system};
                inherit ((inputs.flake-pins-pkgs).packages.${system}) github-linguist epubinfo squasher;
              })
              # Add extra tree-sitter grammars that are not included in nixpkgs
              # yet.
              (_: prev: {
                tree-sitter = prev.tree-sitter.override {
                  extraGrammars = {
                    tree-sitter-astro = {
                      src = inputs.tree-sitter-astro.outPath;
                    };
                  };
                };
              })
            ];
          };

          legacyPackages = pkgs;

          packages =
            {
              inherit emacs-config;

              # test-emacs-config = pkgs.callPackage ./emacs/tests {};

              update-elisp-lock = pkgs.writeShellApplication {
                name = "update-elisp-lock";
                runtimeInputs = [ pkgs.deno ];
                text = ''
                  cd emacs/lock
                  deno run --allow-read --allow-run ${scripts/update-elisp-lock.ts}
                '';
              };
            }
            // (builtins.mapAttrs (
              name: emacs-env:
              emacs-env
              // {
                wrappers = lib.optionalAttrs pkgs.stdenv.isLinux {
                  tmpdir = pkgs.callPackage ./nix/tmpInitDirWrapper.nix { } "emacs-${name}" emacs-env;
                };

                archive-builder = pkgs.makeEmacsTwistArchive {
                  name = "build-emacs-${name}-archive";
                  earlyInitFile = ./emacs/early-init.el;
                  narName = "emacs-profile-${name}.nar";
                  outName = "emacs-profile-${name}-${
                    builtins.substring 0 8 (inputs.self.lastModifiedDate)
                  }-${system}.tar.zstd";
                } emacs-env;

                elpa-archive = pkgs.emacsTwist2Elpa.buildElpaArchiveAsTar { withInstaller = true; } "elpa-archive-${
                  builtins.substring 0 8 (inputs.self.lastModifiedDate)
                }" emacs-env.packageInputs;

                init-file = pkgs.runCommandLocal "init.el" { } ''
                  for file in ${builtins.concatStringsSep " " emacs-env.initFiles}
                  do
                    cat "$file" >> "$out"
                  done
                '';
              }
            ) profiles);

          apps = emacs-config.makeApps { lockDirName = "emacs/lock"; };

          # Set up a pre-commit hook by running `nix develop`.
          devShells = {
            default = pkgs.mkShell {
              inherit
                (inputs.pre-commit-hooks.lib.${system}.run {
                  src = ./.;
                  hooks = import ./hooks.nix { inherit pkgs; };
                })
                shellHook
                ;
            };
          };
        };
    };
}
