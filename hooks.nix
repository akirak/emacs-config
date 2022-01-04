{ pkgs
, flake-no-path
, emacs
, emacsBinaryPackage
}:
let
  emacsForCI =
    (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
      epkgs.org-make-toc
      epkgs.org-ql
    ]);

  emacsConfig = { name, stages, funcName }: {
    enable = true;
    inherit stages;
    inherit name;
    entry = builtins.concatStringsSep " " [
      "${emacsForCI}/bin/emacs"
      "--batch -l ${./scripts/update-emacs-config.el}"
      "-f ${funcName}"
    ];
    files = "emacs-config\.org$";
    pass_filenames = true;
  };

  pushEmacsBinary = pkgs.writeShellScript "push-emacs-binary" ''
    result=$(timeout 3 nix eval --raw .#emacs-full.emacs) \
    && timeout 5 cachix push akirak "$result"
  '';
in
{
  nixpkgs-fmt.enable = true;

  # nix-linter.enable = true;

  flake-no-path = {
    enable = true;
    name = "Ensure that flake.lock does not contain a local path";
    entry = "${flake-no-path}/bin/flake-no-path";
    files = "flake\.lock$";
    pass_filenames = true;
  };

  emacs-config = emacsConfig {
    name = "Sort entries in the Emacs configuration";
    stages = [ "commit" ];
    funcName = "akirak/batch-update-emacs-config";
  };

  emacs-config-contents = emacsConfig {
    name = "Update blocks in the Emacs configuration";
    stages = [ "push" ];
    funcName = "akirak/batch-update-emacs-config-contents";
  };

  push-emacs-binary = {
    enable = true;
    name = "Push the Emacs binary";
    stages = [ "push" ];
    entry = pushEmacsBinary.outPath;
    files = "^flake\.lock$";
    pass_filenames = false;
  };
}
