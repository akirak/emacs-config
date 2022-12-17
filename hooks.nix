{
  pkgs,
  emacsBinaryPackage,
}: let
  emacsConfig = {
    name,
    stages,
    funcName,
  }: {
    enable = true;
    inherit stages;
    inherit name;
    entry = builtins.concatStringsSep " " [
      "${pkgs.emacs-batch}/bin/emacs"
      "--batch -l ${./scripts/update-emacs-config.el}"
      "-f ${funcName}"
    ];
    files = "emacs-config\.org$";
    pass_filenames = true;
  };
in {
  alejandra = {
    enable = true;
    excludes = [
      "emacs/lock/flake\\.nix"
    ];
  };
  # nix-linter.enable = true;

  flake-no-path = {
    enable = true;
    name = "Ensure that flake.lock does not contain a local path";
    entry = "${pkgs.flake-no-path}/bin/flake-no-path";
    files = "flake\.lock$";
    pass_filenames = true;
  };

  emacs-config = emacsConfig {
    name = "Sort entries in the Emacs configuration";
    stages = ["commit"];
    funcName = "akirak/batch-update-emacs-config";
  };

  emacs-config-contents = emacsConfig {
    name = "Update blocks in the Emacs configuration";
    stages = ["push"];
    funcName = "akirak/batch-update-emacs-config-contents";
  };
}
