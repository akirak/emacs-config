{ twist, org-babel, inventories }:
final: prev:
let
  inherit (prev) tangleOrgBabelFile emacsPgtkGcc emacsTwist;
  inherit (org-babel.lib) excludeOrgSubtreesOnHeadlines matchOrgTag;
  inherit (twist.lib { inherit (prev) lib; }) parseSetup;

  orgFile = ./config.org;

  initFile = tangleOrgBabelFile "init.el" orgFile {
    languages = [ "emacs-lisp" ];
    processLines = excludeOrgSubtreesOnHeadlines
      (matchOrgTag "ARCHIVE");
  };

  compatEl = builtins.path {
    name = "compat.el";
    path = ./compat.el;
  };

  makeEmacsConfiguration = initFiles: emacsTwist {
    inventories = [
      {
        type = "melpa";
        path = ./recipes/overrides;
      }
    ] ++ inventories;
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
    full = makeEmacsConfiguration [
      initFile
      compatEl
    ];
  };
}
