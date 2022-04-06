{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  buildInputs = [
    pkgs.bundler
    pkgs.bundix
  ];

  shellHook = ''
    bundle lock
    bundix
  '';
}
