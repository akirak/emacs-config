{
  lib,
  pkgs,
  inputs,
  orgInitSource,
  ...
}@args1:
let
  org-babel = inputs.org-babel.lib;
in
lib.makeOverridable (
  {
    emacsPackage,
    prependToInitFile ? null,
    extraFeatures ? null,
  }:
  let
    initFile = pkgs.tangleOrgBabelFile "init.el" orgInitSource {
      transformLines = org-babel.excludeHeadlines (
        s:
        org-babel.tag "ARCHIVE" s
        || (
          extraFeatures != null
          && org-babel.tag "@extra" s
          && !lib.any (tag: org-babel.tag tag s) extraFeatures
        )
      );
    };
  in
  (import ./make-base-env.nix (
    (lib.removeAttrs args1 [ "orgInitSource" ])
    // {
      inherit emacsPackage;
      initFiles =
        (lib.optional (prependToInitFile != null) (pkgs.writeText "init.el" prependToInitFile))
        ++ [ initFile ];
    }
  )).overrideScope
    (
      lib.composeExtensions inputs.twist-overrides.overlays.twistScope (
        _tself: tsuper: {
          elispPackages = tsuper.elispPackages.overrideScope (import ./elisp-overrides.nix { inherit pkgs; });
        }
      )
    )
)
