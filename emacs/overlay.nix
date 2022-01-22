{ inputs }:
final: prev:
with builtins;
let
  inherit (inputs.twist.lib { inherit (inputs.nixpkgs) lib; }) parseSetup;
  org = inputs.org-babel.lib;
  inherit (prev) lib;

  inherit (inputs.emacs-overlay.overlay final prev) emacsPgtkGcc;
  inherit (inputs.twist.overlay final prev) emacsTwist;
  inherit (inputs.org-babel.overlay final prev) tangleOrgBabelFile;

  releaseVersions = {
    elispTreeSitterVersion = "0.16.1";
    elispTreeSitterLangsVersion = "0.10.13";
  };

  excludeArchive = org.excludeHeadlines (org.tag "ARCHIVE");

  basicInit = tangleOrgBabelFile "init.el" ./emacs-config.org {
    processLines = excludeArchive;
  };

  compatInit = builtins.path {
    name = "compat.el";
    path = ./compat.el;
  };

  selectExtras = extras:
    if extras == true
    then lib.id
    else if isString extras
    then org.selectHeadlines (org.headlineText extras)
    else if isList extras
    then org.selectHeadlines (s: lib.any (substr: org.headlineText substr s) extras)
    else throw "extras must be either null, true, a string, or a list of strings";

  extraInit = extras: tangleOrgBabelFile "extra-init.el" ./extras.org {
    processLines = lines: lib.pipe lines [
      excludeArchive
      (selectExtras extras)
    ];
  };

  configureInitFiles = { compat ? true, extras ? true }:
    [ basicInit ]
    ++ lib.optional compat compatInit
    ++ lib.optional (extras != false) (extraInit extras);

  emacsPackage = emacsPgtkGcc.overrideAttrs (_: { version = "29.0.50"; });

  inventories = import ./inventories.nix inputs;

  makeEmacsProfile = args: (emacsTwist {
    inherit emacsPackage;
    initFiles = configureInitFiles args;
    extraPackages = [
      "setup"
    ];
    initParser = parseSetup { };
    inherit inventories;
    lockDir = ./lock;
    inputOverrides = import ./inputs.nix releaseVersions;
  }).overrideScope' (self: super: {
    elispPackages = super.elispPackages.overrideScope' (import ./overrides.nix releaseVersions {
      pkgs = prev;
      inherit (prev) system;
      emacs = emacsPackage;
    });
  });

  full = makeEmacsProfile {
    compat = true;
    extras = true;
  };

  sandbox = prev.callPackage ./sandbox.nix { };
in
{
  emacsProfiles = lib.extendDerivation true
    {

      basic = sandbox
        {
          themePackage = "doom-themes";
          themeName = "doom-rouge";
        }
        (makeEmacsProfile {
          compat = false;
          extras = false;
        });

      compat = sandbox
        {
          themePackage = "doom-themes";
          themeName = "doom-one";
        }
        (makeEmacsProfile {
          extras = false;
        });

      beancount = sandbox
        {
          themePackage = "doom-themes";
          themeName = "doom-opera-light";
          userEmacsDirectory = "$HOME/beancount/emacs-var";
          extraBubblewrapOptions = [
            "--bind"
            "$HOME/beancount"
            "$HOME/beancount"
            "--bind-try"
            "$HOME/Downloads"
            "$HOME/Downloads"
          ];
        }
        (makeEmacsProfile {
          extras = "beancount";
        });

      readability = sandbox
        {
          themePackage = "poet-theme";
          themeName = "poet";
        }
        (makeEmacsProfile { });

    }
    full;
}
