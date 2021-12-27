{ pkgs, ... }:
{
  home.packages = pkgs.lib.attrVals [
    "zsh"
    "emacs"
  ]
    pkgs;
}
