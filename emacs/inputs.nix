{ elispTreeSitterVersion
, elispTreeSitterLangsVersion
}:
{
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

  # Until https://github.com/legalnonsense/org-visual-outline/pull/5 is merged
  org-visual-indent = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-visual-outline";
      ref = "fix-package-requires";
    };
  };
  org-dynamic-bullets = _: _: {
    origin = {
      type = "github";
      owner = "akirak";
      repo = "org-visual-outline";
      ref = "fix-package-requires";
    };
  };

}
