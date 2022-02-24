{ elispTreeSitterVersion
, elispTreeSitterLangsVersion
}:
{
  taxy-magit-section = _: super: {
    packageRequires = {
      taxy = "0";
    } // super.packageRequires;
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

  twist = _: _: {
    origin = {
      type = "github";
      owner = "emacs-twist";
      repo = "twist.el";
      ref = "develop";
    };
  };
}
