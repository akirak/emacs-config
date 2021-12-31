{ inventories }:
final: prev:
let
  inherit (prev) tangleOrgBabelFile emacsPgtkGcc emacsTwist;

  orgFile = ./config.org;

  initFile = tangleOrgBabelFile "init.el" orgFile {
    languages = [ "emacs-lisp" ];
  };

  makeEmacsConfiguration = initFiles: emacsTwist {
    inventories = [
      {
        type = "melpa";
        path = ./recipes/overrides;
      }
    ] ++ inventories;
    inherit initFiles;
    emacsPackage = emacsPgtkGcc.overrideAttrs (_: { version = "29.0.50"; });
    lockDir = ./sources;
    inputOverrides = { };
  };
in
{
  emacsConfigurations = {
    full = makeEmacsConfiguration [
      initFile
      ./compat.el
    ];
  };
}
