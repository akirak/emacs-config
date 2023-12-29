{
  inputs,
  configurationRevision,
}: final: prev:
with builtins; let
  inherit (inputs.twist.lib {inherit (inputs.nixpkgs) lib;}) parseSetup;
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
      type = "archive";
      url = "https://elpa.nongnu.org/nongnu/";
    }
    {
      type = "gitmodules";
      path = inputs.epkgs.outPath + "/.gitmodules";
    }
  ];

  epkgRepository =
    prev.runCommandLocal "epkg-repository" {
      buildInputs = [
        prev.sqlite
        prev.git
      ];
    } ''
      mkdir $out
      cd $out
      cp -t . "${inputs.epkgs.outPath}/epkg.sql"
      # A workaround to pass `git rev-parse HEAD`
      git init
      git add epkg.sql
      git -c user.name=nouser -c user.email='nouser@localhost' \
        commit -a -m 'Initial commit' --allow-empty
      rm epkg.*
    '';

  defaultTreeSitterGrammars =
    lib.pipe final.tree-sitter-grammars
    [
      (lib.filterAttrs (name: _: name != "recurseForDerivations"))
      builtins.attrValues
    ];

  makeEmacsProfile = {
    extraFeatures,
    prependToInitFile ? null,
    extraInitFiles,
    pgtk ? true,
    withXwidgets,
    extraTreeSitterGrammars ? [],
    nativeCompileAheadDefault ? true,
  }: let
    emacsPackage =
      (
        if pgtk
        then emacsPackages.emacs-pgtk
        else emacsPackages.emacs
      )
      .override (_: (
        if withXwidgets
        then {
          inherit withXwidgets;
        }
        else {}
      ));
  in
    (emacsTwist {
      inherit configurationRevision;
      inherit emacsPackage;
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
          (prev.writeText "init-paths.el" ''
            (with-eval-after-load 'epkg
              (setq epkg-origin-url "${epkgRepository}"))
          '')
          (
            # Based on the fake package in nixpkgs at
            # https://github.com/NixOS/nixpkgs/blob/8f0515dbf74c886b61639ccad5a1ea7c2f51265d/pkgs/applications/editors/emacs/elisp-packages/manual-packages/treesit-grammars/default.nix
            prev.writeText "init-treesit.el" ''
              (add-to-list 'treesit-extra-load-path  "${
                prev.linkFarm "treesit-grammars"
                (
                  map (drv: {
                    name = "lib${
                      lib.removeSuffix "-grammar" (lib.getName drv)
                    }${
                      prev.stdenv.targetPlatform.extensions.sharedLibrary
                    }";
                    path = "${drv}/parser";
                  })
                  (
                    defaultTreeSitterGrammars
                    ++ extraTreeSitterGrammars
                  )
                )
              }/")
            ''
          )
        ]
        # Allow adding private config on specific hosts
        ++ extraInitFiles;
      extraPackages = [
        "setup"
      ];
      inherit initialLibraries;
      initParser = parseSetup {};
      inherit registries;
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
    .overrideScope' (lib.composeExtensions
      inputs.twist-overrides.overlays.twistScope
      (self: super: {
        elispPackages = super.elispPackages.overrideScope' (import ./overrides.nix {
          pkgs = prev;
          inherit (prev) system;
          emacs = emacsPackage;
        });
      }));

  defaultEmacsPackage = emacsPackages.emacs-pgtk;
in {
  emacs-config = lib.makeOverridable makeEmacsProfile {
    extraFeatures = true;
    extraInitFiles = [];
    withXwidgets = false;
  };

  # A configuration with the packages for the Git hooks.
  emacs-batch = emacsTwist {
    emacsPackage = defaultEmacsPackage;
    initFiles = [];
    extraPackages = ["org-ql" "org-make-toc"];
    inherit registries;
    lockDir = ./lock;
  };

  emacsclient =
    inputs.nixpkgs.legacyPackages.${system}.runCommandLocal "emacsclient" {
      propagatedBuildInputs = [defaultEmacsPackage];
    } ''
      mkdir -p $out/bin
      ln -t $out/bin -s ${defaultEmacsPackage}/bin/emacsclient
    '';
}
