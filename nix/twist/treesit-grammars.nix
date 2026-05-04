{ inputs }:
{ emacs, tree-sitter }:
emacs.pkgs.treesit-grammars.with-grammars (
  _:
  # tree-sitter-razor is marked as broken, so it needs to be
  # excluded from the config.
  (builtins.filter (grammar: ((grammar.meta or { }).broken or null) != true) tree-sitter.allGrammars)
  ++ [
    (tree-sitter.buildGrammar {
      language = "astro";
      version = "0";
      src = inputs.tree-sitter-astro.outPath;
    })
  ]
)
