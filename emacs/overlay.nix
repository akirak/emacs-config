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

  extraFile = tangleOrgBabelFile "extra-init.el" ./extras.org {
    processLines = org.excludeHeadlines (org.tag "ARCHIVE");
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
    inputOverrides = { };
  };
in
{
  emacsConfigurations = {
    # Used to generate lock files
    full = makeEmacsConfiguration [
      initFile
      compatEl
      extraFile
    ];
    basic = makeEmacsConfiguration [
      initFile
    ];
    compat = makeEmacsConfiguration [
      initFile
      compatEl
    ];
  };
}
