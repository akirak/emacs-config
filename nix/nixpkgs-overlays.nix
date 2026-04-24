# Copyright (C) 2024-2026 Akira Komamura
# SPDX-License-Identifier: MIT

{ inputs }:
[
  # Bring custom packages into the scope for native dependencies.
  (_: prev:
  let
    inherit (prev.stdenv.hostPlatform) system;
    in
  {
    inherit ((inputs.flake-pins).packages.${system})
      github-linguist
      epubinfo
      squasher
      d2-format
    ;

    lsp-proxy = inputs.lsp-proxy.packages.${system}.default;
  })

  #   # makeEmacsTwistArchive
  #   inputs.archiver.overlays.default

  #   # emacsTwist2Elpa
  #   inputs.twist2elpa.overlays.default
]
