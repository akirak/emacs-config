{ inputs, configurationRevision, }:
final: prev:
with builtins;
let
  inherit (inputs.twist.overlays.default final prev) emacsTwist;
  inherit (inputs.org-babel.overlays.default final prev) tangleOrgBabelFile;
  inherit (prev) system;

  org = inputs.org-babel.lib;
  inherit (prev) lib;

  emacsPackages = (import inputs.flake-pins).packages.${system};

  initialLibraries = (import inputs.flake-pins).data.emacs.libraries;

  # releaseVersions = import ./versions.nix;
  registries = [
    {
      type = "melpa";
      path = inputs.melpa.outPath + "/recipes";
      exclude = [ "async" ];
    }
    {
      type = "elpa";
      path = inputs.gnu-elpa.outPath + "/elpa-packages";
      auto-sync-only = true;
      exclude = [
        # Use tarball, as it contains info
        "org-transclusion"
        "async"
        "persist"
      ];
    }
    {
      type = "archive";
      url = "https://elpa.gnu.org/packages/";
    }
    {
      type = "elpa";
      path = inputs.nongnu-elpa.outPath + "/elpa-packages";
    }
    {
      type = "melpa";
      path = ./recipes;
    }
    {
      type = "archive";
      url = "https://elpa.nongnu.org/nongnu/";
    }
  ];

  # Based on the fake package in nixpkgs at
  # https://github.com/NixOS/nixpkgs/blob/8f0515dbf74c886b61639ccad5a1ea7c2f51265d/pkgs/applications/editors/emacs/elisp-packages/manual-packages/treesit-grammars/default.nix
  treeSitterLoadPath = lib.pipe final.tree-sitter-grammars [
    (lib.filterAttrs (name: _: name != "recurseForDerivations"))
    builtins.attrValues
    (map (drv: {
      # Some grammars don't contain "tree-sitter-" as the prefix,
      # so add it explicitly.
      name = "libtree-sitter-${
          lib.pipe (lib.getName drv) [
            (lib.removeSuffix "-grammar")
            (lib.removePrefix "tree-sitter-")
          ]
        }${prev.stdenv.targetPlatform.extensions.sharedLibrary}";
      path = "${drv}/parser";
    }))
    (prev.linkFarm "treesit-grammars")
  ];

  makeEmacsProfile = { extraFeatures, prependToInitFile ? null, extraInitFiles
    , pgtk ? true, withXwidgets, nativeCompileAheadDefault ? true, }:
    let
      emacsPackage = (if pgtk then
        emacsPackages.emacs-pgtk
      else
        emacsPackages.emacs).override
        (_: (if withXwidgets then { inherit withXwidgets; } else { }));
    in (emacsTwist {
      inherit configurationRevision;
      inherit emacsPackage;
      inherit nativeCompileAheadDefault;
      initFiles = (lib.optional (prependToInitFile != null)
        (prev.writeText "init.el" prependToInitFile)) ++ [
          (tangleOrgBabelFile "init.el" ./emacs-config.org {
            processLines = org.excludeHeadlines (s:
              org.tag "ARCHIVE" s || (if extraFeatures == true then
                false
              else
                (org.tag "@extra" s
                  && !lib.any (tag: org.tag tag s) extraFeatures)));
          })
        ]
        # Allow adding private config on specific hosts
        ++ extraInitFiles;
      extraPackages = [ "setup" ];
      localPackages = [
        # Don't add this package to the lock file
        "akirak"
      ];
      extraSiteStartElisp = ''
        (add-to-list 'treesit-extra-load-path "${treeSitterLoadPath}/")
      '';
      inherit initialLibraries;
      initParser =
        inputs.twist.lib.parseSetup { inherit (inputs.nixpkgs) lib; } { };
      inherit registries;
      lockDir = ./lock;
      inputOverrides = (import ./inputs.nix) // {
        akirak = _: _: {
          src = inputs.nix-filter.lib {
            root = inputs.self;
            include = [ "emacs/lisp" ];
          };
        };
      };
      exportManifest = true;
    }).overrideScope
    (lib.composeExtensions inputs.twist-overrides.overlays.twistScope
      (self: super: {
        elispPackages = super.elispPackages.overrideScope
          (import ./overrides.nix {
            pkgs = prev;
            inherit (prev) system;
            emacs = emacsPackage;
          });
      }));

  defaultEmacsPackage = emacsPackages.emacs-pgtk;
in {
  emacs-config = lib.makeOverridable makeEmacsProfile {
    extraFeatures = true;
    extraInitFiles = [ ];
    withXwidgets = false;
  };

  # A configuration with the packages for the Git hooks.
  emacs-batch = emacsTwist {
    emacsPackage = defaultEmacsPackage;
    initFiles = [ ];
    extraPackages = [ "org-ql" "org-make-toc" ];
    inherit registries;
    lockDir = ./lock;
  };

  emacsclient =
    inputs.nixpkgs.legacyPackages.${system}.runCommandLocal "emacsclient" {
      propagatedBuildInputs = [ defaultEmacsPackage ];
    } ''
      mkdir -p $out/bin
      ln -t $out/bin -s ${defaultEmacsPackage}/bin/emacsclient
    '';
}
