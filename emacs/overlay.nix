{
  inputs,
  emacsPackageForSystem,
  configurationRevision,
}: final: prev:
with builtins; let
  inherit (inputs.twist.lib {inherit (inputs.nixpkgs) lib;}) parseSetup;
  inherit (inputs.twist.overlays.default final prev) emacsTwist;
  inherit (inputs.org-babel.overlays.default final prev) tangleOrgBabelFile;
  inherit (prev) system;

  org = inputs.org-babel.lib;
  inherit (prev) lib;

  emacsPackage = emacsPackageForSystem system;

  # releaseVersions = import ./versions.nix;
  inventories = [
    {
      type = "melpa";
      path = inputs.melpa.outPath + "/recipes";
      exclude = [
        "async"
      ];
    }
    {
      type = "elpa";
      path = inputs.gnu-elpa.outPath + "/elpa-packages";
      auto-sync-only = true;
      exclude = [
        # Use tarball, as it contains info
        "org-transclusion"
        "async"
      ];
    }
    {
      type = "archive";
      url = "https://elpa.gnu.org/packages/";
    }
    {
      type = "archive";
      url = "https://elpa.nongnu.org/nongnu/";
    }
    {
      type = "gitmodules";
      path = inputs.epkgs.outPath + "/.gitmodules";
    }
  ];

  makeEmacsProfile = {
    extraFeatures,
    prependToInitFile ? null,
    extraInitFiles,
    withXwidgets,
    nativeCompileAheadDefault ? true,
  }:
    (emacsTwist {
      inherit configurationRevision;
      emacsPackage =
        if withXwidgets
        then
          emacsPackage.override
          (_: {
            withXwidgets = true;
          })
        else emacsPackage;
      inherit nativeCompileAheadDefault;
      initFiles =
        (lib.optional (prependToInitFile != null) (prev.writeText "init.el" prependToInitFile))
        ++ [
          (tangleOrgBabelFile "init.el" ./emacs-config.org {
            processLines = org.excludeHeadlines (s:
              org.tag "ARCHIVE" s
              || (
                if extraFeatures == true
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
      initParser = parseSetup {};
      inherit inventories;
      lockDir = ./lock;
      inputOverrides =
        (import ./inputs.nix)
        // {
          akirak = _: _: {
            src = inputs.nix-filter.lib {
              root = inputs.self;
              include = [
                "emacs/lisp"
              ];
            };
          };
        };
      exportManifest = true;
    })
    .overrideScope' (self: super: {
      elispPackages = super.elispPackages.overrideScope' (import ./overrides.nix {
        pkgs = prev;
        inherit (prev) system;
        emacs = emacsPackage;
      });
    });
in {
  emacs-config = lib.makeOverridable makeEmacsProfile {
    extraFeatures = true;
    extraInitFiles = [];
    withXwidgets = false;
  };

  # A configuration with the packages for the Git hooks.
  emacs-batch = emacsTwist {
    inherit emacsPackage;
    initFiles = [];
    extraPackages = ["org-ql" "org-make-toc"];
    inherit inventories;
    lockDir = ./lock;
  };

  emacsclient =
    inputs.nixpkgs.legacyPackages.${system}.runCommandLocal "emacsclient" {
      propagatedBuildInputs = [emacsPackage];
    } ''
      mkdir -p $out/bin
      ln -t $out/bin -s ${emacsPackage}/bin/emacsclient
    '';
}
