# Copyright (C) 2022-2025 Akira Komamura
# SPDX-License-Identifier: MIT

{
  org = _: _: {
    origin = {
      # git.savannah.org is unstable
      type = "github";
      owner = "elpa-mirrors";
      repo = "org-mode";
      ref = "bugfix";
    };
  };

  bufler = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "bufler.el";
      ref = "fix-cl-macs";
    };
  };

  magit-delta = _: super: {
    packageRequires = super.packageRequires // {
      dash = "0";
      # Override the snapshot version dependency.
      magit = "3";
    };
  };

  # Quite a few dired extension packages have missing dependencies.
  dired-collapse = _: super: {
    packageRequires = {
      dash = "0";
      f = "0";
      dired-hacks-utils = "0";
    } // super.packageRequires;
  };
  dired-filter = _: super: {
    packageRequires = {
      dired-hacks-utils = "0";
      f = "0";
    } // super.packageRequires;
  };
  dired-open = _: super: {
    packageRequires = {
      dired-hacks-utils = "0";
    } // super.packageRequires;
  };

  ob-graphql = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "ob-graphql";
      ref = "graphql-dep";
    };
  };

  org-bookmark-heading = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-bookmark-heading";
      ref = "fix-pcase";
    };
  };

  org-dog = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-dog";
      ref = "develop";
    };
  };
  org-dog-embark = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-dog";
      ref = "develop";
    };
  };
  org-dog-export = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-dog";
      ref = "develop";
    };
  };
  consult-org-dog = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-dog";
      ref = "develop";
    };
  };
  org-dog-facade = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-dog";
      ref = "develop";
    };
  };
  octopus = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-dog";
      ref = "develop";
    };
  };

  lispy = _: super: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "lispy";
      ref = "safe-print";
    };
    files = builtins.removeAttrs super.files [
      # le-js depends on indium, which I don't want to install.
      "le-js.el"
      # lispy-occur depends on swiper
      "lispy-occur.el"
    ];
    packageRequires =
      (builtins.removeAttrs super.packageRequires [
        "swiper"
        "ace-window"
      ])
      // {
        avy = "0";
      };
  };

  org-memento = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-memento";
      ref = "develop";
    };
  };

  persist = _: super: {
    files = builtins.removeAttrs super.files [ "persist.texi" ];
  };

  org-autolist = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-autolist";
      ref = "patch-org-element";
    };
  };

  neotree = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "emacs-neotree";
      ref = "multi";
    };
  };

  taxy = _: super: {
    files = builtins.removeAttrs super.files [ "NOTES.org" ];
  };

  taxy-magit-section = _: super: {
    files = builtins.removeAttrs super.files [ "NOTES.org" ];
  };

  plz = _: super: { files = builtins.removeAttrs super.files [ "NOTES.org" ]; };

  daemons = _: super: {
    packageRequires = {
      s = "0";
    } // super.packageRequires;
  };

  lean4-mode = _: super: {
    packageRequires = builtins.removeAttrs super.packageRequires [
      "lsp-mode"
    ];
    files = builtins.removeAttrs super.files [
      "lean4-info.el"
      "lean4-fringe.el"
      "lean4-lsp.el"
    ];
    origin = {
      type = "github";
      owner = "akirak";
      repo = "lean4-mode";
      ref = "eglot";
    };
    # origin = {
    #   type = "github";
    #   owner = "leanprover-community";
    #   repo = "lean4-mode";
    #   ref = "milestone-03-breaking-refactor";
    # };
  };
}
