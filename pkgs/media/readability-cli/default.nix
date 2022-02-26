{ pkgs }:
let
  composition = (pkgs.callPackage ./composition.nix {
    inherit (pkgs) system;
    nodejs = pkgs.nodejs-14_x;
  }).readability-cli;
in
pkgs.runCommandLocal "readable"
{
  propagateBuildInputs = [ composition ];
} ''
  mkdir -p $out/bin
  ln -s ${composition}/bin/readable $out/bin/readable
''
