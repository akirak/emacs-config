{ lib, inputs }:
lib.genAttrs
  [
    "zsh-enhancd"
    "zsh-fzy"
    "zsh-nix-shell"
    "zsh-fast-syntax-highlighting"
  ]
  (name: inputs.${name}.outPath)
