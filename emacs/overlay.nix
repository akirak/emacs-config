{ inputs, nixpkgs }:
final: prev:
with builtins;
let
  inherit (inputs.twist.lib { inherit (inputs.nixpkgs) lib; }) parseSetup;
  inherit (inputs.twist.overlay final prev) emacsTwist;
  inherit (inputs.org-babel.overlay final prev) tangleOrgBabelFile;

  org = inputs.org-babel.lib;
  inherit (prev) lib;

  # Use a pinned nixpkgs to prevent the rebuild of Emacs on updating nixpkgs for the system.
  pkgsForEmacs = import nixpkgs {
    inherit (prev) system;
    overlays = [
      inputs.emacs-overlay.overlay
    ];
  };
  emacsPackage =
    pkgsForEmacs.emacsPgtkGcc.overrideAttrs (_: { version = "29.0.50"; });

  releaseVersions = import ./versions.nix;
  inventories = import ./inventories.nix inputs;

  makeEmacsProfile =
    { extraFeatures
    , extraInitFiles
    , withXwidgets
    }: (emacsTwist {
      emacsPackage =
        if withXwidgets
        then
          emacsPackage.override
            (_: {
              inherit (pkgsForEmacs) webkitgtk;
              withXwidgets = true;
            })
        else emacsPackage;
      initFiles = [
        (tangleOrgBabelFile "init.el" ./emacs-config.org {
          processLines = org.excludeHeadlines (s:
            org.tag "ARCHIVE" s
              ||
              (if extraFeatures == true
              then false
              else
                (org.tag "@extra" s
                  && ! lib.any (tag: org.tag tag s) extraFeatures)
              ));
        })
      ]
      # Allow adding private config on specific hosts
      ++ extraInitFiles;
      extraPackages = [
        "setup"
      ];
      initParser = parseSetup { };
      inherit inventories;
      lockDir = ./lock;
      inputOverrides = import ./inputs.nix releaseVersions;
    }).overrideScope' (self: super: {
      elispPackages = super.elispPackages.overrideScope' (import ./overrides.nix releaseVersions {
        pkgs = prev;
        inherit (prev) system;
        emacs = emacsPackage;
      });
    });
in
{
  emacs-config = lib.makeOverridable makeEmacsProfile {
    extraFeatures = true;
    extraInitFiles = [ ];
    withXwidgets = false;
  };

  # A configuration with the packages for the Git hooks.
  emacs-batch = emacsTwist {
    inherit emacsPackage;
    initFiles = [ ];
    extraPackages = [ "org-ql" "org-make-toc" ];
    inherit inventories;
    lockDir = ./lock;
  };
}
