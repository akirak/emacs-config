{
  elispTreeSitterVersion,
  elispTreeSitterLangsVersion,
}: {
  taxy-magit-section = _: super: {
    packageRequires =
      {
        taxy = "0";
      }
      // super.packageRequires;
  };
  bufler = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "bufler.el";
      ref = "fix-cl-macs";
    };
  };
  tsc = _: _: {
    origin = {
      type = "github";
      owner = "emacs-tree-sitter";
      repo = "elisp-tree-sitter";
      ref = elispTreeSitterVersion;
    };
  };
  tree-sitter = _: _: {
    origin = {
      type = "github";
      owner = "emacs-tree-sitter";
      repo = "elisp-tree-sitter";
      ref = elispTreeSitterVersion;
    };
  };
  tree-sitter-langs = _: _: {
    origin = {
      type = "github";
      owner = "emacs-tree-sitter";
      repo = "tree-sitter-langs";
      ref = elispTreeSitterLangsVersion;
    };
  };

  graphviz-dot-mode = _: super: {
    files = builtins.removeAttrs super.files ["company-graphviz-dot.el"];
  };

  # ghelp is not a proper MELPA package yet, and it needs workarounds.
  ghelp-helpful = _: _: {
    packageRequires = {
      ghelp = "0";
      helpful = "0";
    };
  };
  ghelp-eglot = _: _: {
    packageRequires = {
      ghelp = "0";
      eglot = "0";
    };
  };

  ghub = _: super: {
    files = builtins.removeAttrs super.files [".dir-locals.el"];
  };

  # Quite a few dired extension packages have missing dependencies.
  dired-collapse = _: super: {
    packageRequires =
      {
        dash = "0";
        f = "0";
        dired-hacks-utils = "0";
      }
      // super.packageRequires;
  };
  dired-filter = _: super: {
    packageRequires =
      {
        dired-hacks-utils = "0";
        f = "0";
      }
      // super.packageRequires;
  };
  dired-open = _: super: {
    packageRequires =
      {
        dired-hacks-utils = "0";
      }
      // super.packageRequires;
  };
  dired-hacks-utils = _: super: {
    packageRequires =
      {
        dash = "0";
      }
      // super.packageRequires;
  };
  # I won't use packages that depend on direx.
  # dired-k = _: super: {
  #   packageRequires = {
  #     direx = "0";
  #   } // super.packageRequires;
  # };

  # Fix a performance issue described in
  # https://github.com/noctuid/link-hint.el/issues/206
  link-hint = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "link-hint.el";
      ref = "skip-invisible";
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

  twist = _: _: {
    origin = {
      type = "github";
      owner = "emacs-twist";
      repo = "twist.el";
      ref = "develop";
    };
  };
  nix26 = _: _: {
    origin = {
      type = "github";
      owner = "emacs-twist";
      repo = "nix26.el";
      ref = "develop";
    };
  };
  eglot = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "eglot";
      ref = "develop";
    };
  };
  fanyi = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "fanyi.el";
      ref = "possibly-fix";
    };
  };
  dash-docs = _: super: {
    files = builtins.removeAttrs super.files [
      "use-package-dash-docs.el"
    ];
  };
}
