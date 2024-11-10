{ lib, inputs, ... }:
let
  org-babel = inputs.org-babel.lib;

  initOrg = ./emacs-config.org;

  makeInitFileWithFilter =
    processLines:
    org-babel.tangleOrgBabel {
      inherit processLines;
    } (builtins.readFile initOrg);

  withoutArchivedSubtrees = org-babel.excludeHeadlines (org-babel.tag "ARCHIVE");

  withExtraTags =
    features:
    org-babel.excludeHeadlines (
      s:
      org-babel.tag "ARCHIVE" s
      || (org-babel.tag "@extra" s && !lib.any (tag: org-babel.tag tag s) features)
    );

  twist-args = {
    # Define the configuration revision based on the repository's Git revision.
    configurationRevision = import ./nix/lib/configurationRevision.nix inputs.self;

    initialLibraries = (import inputs.flake-pins).data.emacs.libraries;

    registries = [
      {
        type = "melpa";
        path = ./recipes;
      }
    ] ++ (import ./nix/twist/registries.nix { inherit inputs; });

    lockDir = ./lock;

    initParser = inputs.twist.lib.parseSetup { inherit (inputs.nixpkgs) lib; } { };

    inputOverrides = (import ./nix/twist/inputs.nix) // {
      akirak = _: _: {
        src = inputs.nix-filter.lib {
          root = inputs.self;
          include = [ "lisp" ];
        };
      };
    };

    # Packages that are not defined in init.el (setup cannot depend on itself)
    extraPackages = [ "setup" ];
    # Packages that should not be added to the lock file
    localPackages = [
      # Don't add this package to the lock file
      "akirak"
    ];

    exportManifest = true;
  };

  makeConfig =
    {
      pkgs,
      emacsPackage ? (import inputs.flake-pins).packages.${pkgs.system}.emacs-pgtk,
      nativeCompileAheadDefault ? true,
      features ? [ ],
      initFile ? (pkgs.writeText "init.el" (makeInitFileWithFilter (withExtraTags features))),
      prependToInitFile ? null,
      initFiles ?
        (lib.optional (prependToInitFile != null) (pkgs.writeText "init.el" prependToInitFile))
        ++ [
          initFile
        ],
    }:
    (inputs.twist.lib.makeEnv (
      twist-args
      // {
        inherit
          pkgs
          emacsPackage
          nativeCompileAheadDefault
          initFiles
          ;
        # Based on https://github.com/jordanisaacs/emacs-config/commit/b3311f31150e7bf015563f35b25cf769d847bfa1#diff-206b9ce276ab5971a2489d75eb1b12999d4bf3843b7988cbe8d687cfde61dea0R63
        extraSiteStartElisp = ''
          (add-to-list 'treesit-extra-load-path "${pkgs.emacs.pkgs.treesit-grammars.with-all-grammars}/lib/")
        '';
      }
    )).overrideScope
      (
        lib.composeExtensions inputs.twist-overrides.overlays.twistScope (
          _tself: tsuper: {
            elispPackages = tsuper.elispPackages.overrideScope (
              import ./nix/twist/overrides.nix {
                inherit pkgs;
              }
            );
          }
        )
      );

  overlays = import ./nix/nixpkgs-overlays.nix { inherit inputs; };
in
{
  flake = {
    homeModules.twist = {
      imports = [
        inputs.twist.homeModules.emacs-twist
        (import ./modules/home-module.nix makeConfig { inherit overlays; })
      ];
    };
  };

  perSystem =
    {
      pkgs,
      lib,
      system,
      emacs-config,
      ...
    }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system overlays;
      };

      _module.args.emacs-config = makeConfig {
        inherit pkgs;
        initFile = pkgs.writeText "init.el" (makeInitFileWithFilter withoutArchivedSubtrees);
      };

      packages = {
        inherit emacs-config;

        emacsclient =
          inputs.nixpkgs.legacyPackages.${system}.runCommandLocal "emacsclient"
            { propagatedBuildInputs = [ emacs-config.emacs ]; }
            ''
              mkdir -p $out/bin
              ln -t $out/bin -s ${emacs-config.emacs}/bin/emacsclient
            '';

        # An extra utility for updating elisp packages by one commit per
        # author.
        update-elisp-lock = pkgs.writeShellApplication {
          name = "update-elisp-lock";
          runtimeInputs = [ pkgs.deno ];
          text = ''
            cd lock
            deno run --allow-read --allow-run ${scripts/update-elisp-lock.ts}
          '';
        };

        emacs-pgtk-on-tmpdir =
          pkgs.callPackage ./nix/lib/tmpInitDirWrapper.nix { } "emacs-tmpdir"
            (makeConfig {
              inherit pkgs;
              emacsPackage = (import inputs.flake-pins).packages.${pkgs.system}.emacs-pgtk;
            });

        emacs-on-tmpdir = pkgs.callPackage ./nix/lib/tmpInitDirWrapper.nix { } "emacs-tmpdir" (makeConfig {
          inherit pkgs;
          emacsPackage = (import inputs.flake-pins).packages.${pkgs.system}.emacs;
        });

        # archive-builder = pkgs.makeEmacsTwistArchive {
        #   name = "build-emacs-${name}-archive";
        #   earlyInitFile = ./emacs/early-init.el;
        #   narName = "emacs-profile-${name}.nar";
        #   outName = "emacs-profile-${name}-${
        #     builtins.substring 0 8 (inputs.self.lastModifiedDate)
        #   }-${system}.tar.zstd";
        # } emacs-env;

        # elpa-archive = pkgs.emacsTwist2Elpa.buildElpaArchiveAsTar { withInstaller = true; } "elpa-archive-${
        #   builtins.substring 0 8 (inputs.self.lastModifiedDate)
        # }" emacs-env.packageInputs;

        # init-file = pkgs.runCommandLocal "init.el" { } ''
        #   for file in ${builtins.concatStringsSep " " emacs-env.initFiles}
        #   do
        #     cat "$file" >> "$out"
        #   done
        # '';

      };

      apps = emacs-config.makeApps { lockDirName = "lock"; };

      checks = {
        # Check if the elisp packages are successfully built.
        elisp-packages = emacs-config.overrideScope (
          _tself: _tsuper: {
            executablePackages = [ ];
          }
        );

        # depsCheck = emacs-config.depsCheck;
      };
    };
}