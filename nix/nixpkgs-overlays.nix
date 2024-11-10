{ inputs }:
[
  # Bring custom packages into the scope for native dependencies.
  (_: prev: {
    inherit ((inputs.flake-pins-pkgs).packages.${prev.system})
      github-linguist
      epubinfo
      squasher
      d2-format
      ;
  })

  # Add extra tree-sitter grammars that are not included in nixpkgs
  # yet.
  (_: prev: {
    tree-sitter = prev.tree-sitter.override {
      extraGrammars = {
        tree-sitter-astro = {
          src = inputs.tree-sitter-astro.outPath;
        };
      };
    };
  })

  #   # makeEmacsTwistArchive
  #   inputs.archiver.overlays.default

  #   # emacsTwist2Elpa
  #   inputs.twist2elpa.overlays.default
]
