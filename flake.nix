{
  inputs = {
    # Should be updated from flake-pins: <https://github.com/akirak/flake-pins>
    utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-filter.url = "github:numtide/nix-filter";

    flake-pins = {
      url = "github:akirak/flake-pins";
      flake = false;
    };

    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    # Emacs
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    org-babel.url = "github:emacs-twist/org-babel";
    twist.url = "github:emacs-twist/twist.nix";
    melpa = {
      url = "github:akirak/melpa/akirak";
      flake = false;
    };
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    epkgs = {
      url = "github:emacsmirror/epkgs";
      flake = false;
    };

    # pre-commit
    flake-no-path = {
      url = "github:akirak/flake-no-path";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };

    my-overlay.url = "github:akirak/nixpkgs-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    flake-parts,
    utils,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = {
        config,
        system,
        pkgs,
        final,
        ...
      }: let
        inherit (final) emacs-config;
      in {
        overlayAttrs =
          {
            coq = inputs.unstable.legacyPackages.${final.system}.coq;
            coq-lsp = inputs.unstable.legacyPackages.${final.system}.coqPackages.coq-lsp;
          }
          // (
            inputs.my-overlay.overlays.default final pkgs
          )
          // (
            import ./emacs/overlay.nix {
              inherit inputs;
              emacsPackageForSystem = system:
                (import inputs.flake-pins).packages.${system}.emacs-pgtk;
            }
            final
            pkgs
          );

        packages = {
          # tryout-emacs = emacsSandboxed {
          #   name = "tryout-emacs";
          #   nativeCompileAheadDefault = false;
          #   automaticNativeCompile = false;
          #   enableOpinionatedSettings = false;
          #   extraFeatures = [];
          #   protectHome = false;
          #   shareNet = false;
          #   inheritPath = false;
          # };

          inherit emacs-config;

          # test-emacs-config = pkgs.callPackage ./emacs/tests {};

          update-elisp = pkgs.writeShellScriptBin "update-elisp" ''
            nix flake lock --update-input melpa --update-input gnu-elpa
            cd emacs/lock
            bash ./update.bash "$@"
          '';

          update-elisp-lock = pkgs.writeShellApplication {
            name = "update-elisp-lock";
            runtimeInputs = [
              pkgs.deno
            ];
            text = ''
              cd emacs/lock
              deno run --allow-read --allow-run ${scripts/update-elisp-lock.ts}
            '';
          };
        };

        apps = emacs-config.makeApps {
          lockDirName = "emacs/lock";
        };

        # Set up a pre-commit hook by running `nix develop`.
        devShells = {
          default = pkgs.mkShell {
            inherit
              (inputs.pre-commit-hooks.lib.${system}.run {
                src = ./.;
                hooks = import ./hooks.nix {
                  inherit pkgs;
                  emacsBinaryPackage = "emacs-config.emacs";
                };
              })
              shellHook
              ;
          };
        };
      };
    };
}
