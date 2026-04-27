{
  lib,
  inputs,
  pkgs,
  recipesDir,
  lockDir,
  configurationRevision,
  emacsPackage,
  initFiles,
}:
inputs.twist.lib.makeEnv {
  inherit
    pkgs
    emacsPackage
    configurationRevision
    lockDir
    initFiles
    ;

  exportManifest = true;
  nativeCompileAheadDefault = true;

  initParser = inputs.twist.lib.parseSetup { inherit lib; } { };

  initialLibraries = (import inputs.flake-pins).data.emacs.libraries;

  # Packages that are not defined in init.el (setup cannot depend on itself)
  extraPackages = [ "setup" ];

  # Packages that should not be added to the lock file
  localPackages = [
    # Don't add this package to the lock file
    "akirak"
    "lsp-proxy"
  ];

  registries = [
    {
      type = "melpa";
      path = recipesDir;
    }
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
      url = "https://elpa.gnu.org/devel/";
    }
    {
      type = "elpa";
      path = inputs.nongnu-elpa.outPath + "/elpa-packages";
    }
    {
      type = "archive";
      url = "https://elpa.nongnu.org/nongnu-devel/";
    }
  ];

  inputOverrides = (import ./input-overrides.nix) // {
    akirak = _: _: {
      src = inputs.nix-filter.lib {
        root = inputs.self;
        include = [ "lisp" ];
      };
    };

    lsp-proxy = _: _super: {
      src = inputs.lsp-proxy.outPath;
    };
  };

  # Based on https://github.com/jordanisaacs/full-emacs-env/commit/b3311f31150e7bf015563f35b25cf769d847bfa1#diff-206b9ce276ab5971a2489d75eb1b12999d4bf3843b7988cbe8d687cfde61dea0R63
  extraSiteStartElisp = ''
    (add-to-list 'treesit-extra-load-path "${
      pkgs.callPackage (import ./treesit-grammars.nix { inherit inputs; }) { }
    }/lib/")
  '';
}
