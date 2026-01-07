# Copyright (C) 2024 Akira Komamura
# SPDX-License-Identifier: MIT

{ inputs }:
[
  {
    type = "melpa";
    path = inputs.melpa.outPath + "/recipes";
    exclude = [ "async" ];
  }

  {
    type = "elpa";
    path = inputs.gnu-elpa.outPath + "/elpa-packages";
    auto-sync-only = true;
    exclude = [
      # Use tarball, as it contains info
      "org-transclusion"
      "async"
      "persist"
    ];
  }

  {
    type = "archive";
    url = "https://elpa.gnu.org/devel/";
  }

  {
    type = "elpa";
    path = inputs.nongnu-elpa.outPath + "/elpa-packages";
  }

  {
    type = "archive";
    url = "https://elpa.nongnu.org/nongnu-devel/";
  }
]
