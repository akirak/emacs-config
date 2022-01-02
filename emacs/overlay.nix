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

  makeEmacsConfiguration = initFiles: emacsTwist {
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
    initParser = parseSetup;
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
  };
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
