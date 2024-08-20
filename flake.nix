{
  inputs = {
    # Should be updated from flake-pins: <https://github.com/akirak/flake-pins>
    utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-filter.url = "github:numtide/nix-filter";

    flake-pins = {
      url = "github:akirak/flake-pins";
      flake = false;
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

    archiver.url = "github:emacs-twist/twist-archiver";

    # pre-commit
    flake-no-path = {
      url = "github:akirak/flake-no-path";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };

    my-overlay.url = "github:akirak/nixpkgs-overlay";

    tree-sitter-astro = {
      url = "github:virchau13/tree-sitter-astro";
      flake = false;
    };
    tree-sitter-gleam = {
      url = "github:gleam-lang/tree-sitter-gleam";
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
      utils,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [ inputs.flake-parts.flakeModules.easyOverlay ];

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
          final,
          ...
        }:
        let
          inherit (pkgs) lib;
          inherit (final) emacs-config;
          inherit (builtins) substring;
          profiles = import ./emacs/profiles.nix {
            inherit lib;
            defaultFeatures = import inputs.emacs-config-features;
          } emacs-config;
        in
        {
          overlayAttrs =
            {
              coq = inputs.nixpkgs.legacyPackages.${final.system}.coq;
              coq-lsp = inputs.nixpkgs.legacyPackages.${final.system}.coqPackages.coq-lsp;
              flake-no-path = inputs.flake-no-path.defaultPackage.${system};
              inherit (inputs.my-overlay.packages.${final.system}) github-linguist epubinfo squasher;
              # This will indirectly override tree-sitter-grammars as wells
              tree-sitter = pkgs.tree-sitter.override {
                extraGrammars = {
                  tree-sitter-astro = {
                    src = inputs.tree-sitter-astro.outPath;
                  };
                  tree-sitter-gleam = {
                    src = inputs.tree-sitter-gleam.outPath;
                  };
                };
              };
            }
            // (import ./emacs/overlay.nix {
              inherit inputs;
              configurationRevision = "${substring 0 8 self.lastModifiedDate}.${
                if self ? rev then substring 0 7 self.rev else "dirty"
              }";
            } final pkgs);

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

              build-packages = pkgs.writeShellApplication {
                name = "build-packages";
                runtimeInputs = [
                  pkgs.nix-eval-jobs
                  pkgs.jq
                  pkgs.bc
                ];
                text = ''
                  system=$(nix eval --expr builtins.currentSystem --impure --raw)
                  flake="#packages.$system.emacs-config.elispPackages"
                  ncpus="$(nproc)"
                  if [[ -v CACHIX_NAME ]]
                  then
                    cachix="''${CACHIX_NAME}"
                  fi

                  start=$(cut -d' ' -f1 /proc/uptime)

                  function log() {
                    local time
                    time=$(cut -d' ' -f1 /proc/uptime)
                    local duration
                    duration="$(echo "$time - $start" | bc)"
                    printf >&2 "[%6.1f s] %s\n" "$duration" "$*"
                  }

                  nix-eval-jobs \
                    --workers "$ncpus" \
                    --gc-roots-dir gcroot \
                    --flake "$flake" \
                    --quiet \
                    --check-cache-status \
                    | while read -r line; do
                        p=$(jq -r .drvPath <<< "$line")^out
                        if [[ $(jq -r .cacheStatus <<< "$line") = notBuilt ]]
                        then
                          log "Building $p"
                          if [[ -v cachix ]]
                          then
                            cachix watch-exec "$cachix" \
                              nix -- build "$p" --no-link --print-build-logs
                          else
                            nix build "$p" --no-link --print-build-logs
                          fi
                        else
                          log "Skipping $p"
                        fi
                      done

                  log "Finished building Emacs Lisp packages."
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

                archive-builder = (inputs.archiver.overlays.default final pkgs).makeEmacsTwistArchive {
                  name = "build-emacs-${name}-archive";
                  earlyInitFile = ./emacs/early-init.el;
                  narName = "emacs-profile-${name}.nar";
                  outName = "emacs-profile-${name}-${
                    builtins.substring 0 8 (inputs.self.lastModifiedDate)
                  }-${system}.tar.zstd";
                } emacs-env;
              }
            ) profiles);

          apps = emacs-config.makeApps { lockDirName = "emacs/lock"; };

          # Set up a pre-commit hook by running `nix develop`.
          devShells = {
            default = pkgs.mkShell {
              inherit
                (inputs.pre-commit-hooks.lib.${system}.run {
                  src = ./.;
                  hooks = import ./hooks.nix { pkgs = final; };
                })
                shellHook
                ;
            };
          };
        };
    };
}
