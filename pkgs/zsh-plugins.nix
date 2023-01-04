{
  lib,
  inputs,
}:
lib.genAttrs
[
  "zsh-enhancd"
  "zsh-fzy"
  "zsh-nix-shell"
  "zsh-fast-syntax-highlighting"
  "zsh-history-filter"
]
(name: inputs.${name}.outPath)
