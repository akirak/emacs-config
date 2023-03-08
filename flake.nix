{
  inputs = {
    # Should be updated from flake-pins: <https://github.com/akirak/flake-pins>
    utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";
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
    flake-utils-plus,
    utils,
    ...
  } @ inputs: let
    emacsOverlay = import ./emacs/overlay.nix {
      inherit inputs;
      emacsPackageForSystem = system:
        (import inputs.flake-pins).packages.${system}.emacs-pgtk;
    };
  in
    flake-utils-plus.lib.mkFlake {
      inherit self inputs;

      supportedSystems = ["x86_64-linux"];

      sharedOverlays = [
        inputs.my-overlay.overlays.default
        inputs.flake-no-path.overlay
        emacsOverlay
        (final: _: {
          coq = inputs.unstable.legacyPackages.${final.system}.coq;
          coq-lsp = inputs.unstable.legacyPackages.${final.system}.coqPackages.coq-lsp;
        })
      ];

      outputsBuilder = channels: let
        inherit (channels.nixpkgs) emacs-config emacsSandboxed;
      in {
        packages = {
          tryout-emacs = emacsSandboxed {
            name = "tryout-emacs";
            nativeCompileAheadDefault = false;
            automaticNativeCompile = false;
            enableOpinionatedSettings = false;
            extraFeatures = [];
            protectHome = false;
            shareNet = false;
            inheritPath = false;
          };

          inherit (channels.nixpkgs) readability-cli;

          inherit emacs-config;

          test-emacs-config = channels.nixpkgs.callPackage ./emacs/tests {};

          update-elisp = channels.nixpkgs.writeShellScriptBin "update-elisp" ''
            nix flake lock --update-input melpa --update-input gnu-elpa
            cd emacs/lock
            bash ./update.bash "$@"
          '';

          update-elisp-lock = channels.nixpkgs.writeShellApplication {
            name = "update-elisp-lock";
            runtimeInputs = [
              channels.nixpkgs.deno
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
          default = channels.nixpkgs.mkShell {
            inherit
              (inputs.pre-commit-hooks.lib.${channels.nixpkgs.system}.run {
                src = ./.;
                hooks = import ./hooks.nix {
                  pkgs = channels.nixpkgs;
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
