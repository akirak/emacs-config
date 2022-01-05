{ twist, org-babel, inventories }:
final: prev:
let
  inherit (prev) tangleOrgBabelFile emacsPgtkGcc emacsTwist;
  org = org-babel.lib;
  inherit (twist.lib { inherit (prev) lib; }) parseSetup;

  initFile = tangleOrgBabelFile "init.el" ./emacs-config.org {
    processLines = org.excludeHeadlines (org.tag "ARCHIVE");
  };

  compatEl = builtins.path {
    name = "compat.el";
    path = ./compat.el;
  };

  extraConfigFile = ./extras.org;

  extraFile = f: tangleOrgBabelFile "extra-init.el" ./extras.org {
    processLines = lines: prev.lib.pipe lines [
      (org.excludeHeadlines (org.tag "ARCHIVE"))
      f
    ];
  };

  makeEmacsConfiguration = initFiles: (emacsTwist {
    inventories = [
      {
        type = "melpa";
        path = ./recipes/overrides;
      }
    ] ++ inventories ++ [
      {
        type = "melpa";
        path = ./recipes/fallbacks;
      }
    ];
    inherit initFiles;
    extraPackages = [
      "setup"
    ];
    initParser = parseSetup { };
    emacsPackage = emacsPgtkGcc.overrideAttrs (_: { version = "29.0.50"; });
    lockDir = ./sources;
    inputOverrides = {
      bufler = _: _: {
        origin = {
          type = "github";
          owner = "akirak";
          repo = "bufler.el";
          ref = "fix-cl-macs";
        };
      };
    };
  }).overrideScope' (self: super: {
    elispPackages = super.elispPackages // {
      vterm = super.elispPackages.vterm.overrideAttrs (old: {
        # Based on the configuration in nixpkgs available at the following URL:
        # https://github.com/NixOS/nixpkgs/blob/af21d41260846fb9c9840a75e310e56dfe97d6a3/pkgs/applications/editors/emacs/elisp-packages/melpa-packages.nix#L483
        nativeBuildInputs = [ final.cmake final.gcc ];
        buildInputs = old.buildInputs ++ [ final.libvterm-neovim ];
        cmakeFlags = [
          "-DEMACS_SOURCE=${self.emacs.src}"
        ];
        preBuild = ''
          cmake
          make
          install -m444 -t . ../*.so
          install -m600 -t . ../*.el
          cp -r -t . ../etc
          rm -rf {CMake*,build,*.c,*.h,Makefile,*.cmake}
        '';
      });
    };
  });
in
{
  emacsConfigurations = {
    # Used to generate lock files
    full = makeEmacsConfiguration [
      initFile
      compatEl
      (extraFile prev.lib.id)
    ];
    basic = makeEmacsConfiguration [
      initFile
    ];
    compat = makeEmacsConfiguration [
      initFile
      compatEl
    ];
    beancount = makeEmacsConfiguration [
      initFile
      compatEl
      (extraFile (org.selectHeadlines (org.headlineText "beancount")))
    ];
  };
}
