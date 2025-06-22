# Copyright (C) 2024 Akira Komamura
# SPDX-License-Identifier: MIT

{ inputs }:
[
  # Bring custom packages into the scope for native dependencies.
  (_: prev: {
    inherit ((inputs.flake-pins).packages.${prev.system})
      github-linguist
      epubinfo
      squasher
      d2-format
      ;
  })

  #   # makeEmacsTwistArchive
  #   inputs.archiver.overlays.default

  #   # emacsTwist2Elpa
  #   inputs.twist2elpa.overlays.default
]
