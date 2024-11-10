{ pkgs }:
let
  make-emacs-hook =
    {
      name,
      stages,
      funcName,
    }:
    {
      enable = true;
      inherit stages;
      inherit name;
      entry = builtins.concatStringsSep " " [
        "${pkgs.emacs-with-pkgs}/bin/emacs"
        "--batch -l ${./utils.el}"
        "-f ${funcName}"
      ];
      files = "emacs-config\.org$";
      pass_filenames = true;
    };
in
{
  nixfmt-rfc-style = {
    enable = true;
    excludes = [ "lock/flake\\.nix" ];
  };

  flake-no-path = {
    enable = true;
    name = "Ensure that flake.lock does not contain a local path";
    entry = "${pkgs.flake-no-path}/bin/flake-no-path";
    files = "flake\.lock$";
    pass_filenames = true;
  };

  emacs-config = make-emacs-hook {
    name = "Sort entries in the Emacs configuration";
    stages = [ "commit" ];
    funcName = "akirak/batch-update-emacs-config";
  };

  emacs-config-contents = make-emacs-hook {
    name = "Update blocks in the Emacs configuration";
    stages = [ "push" ];
    funcName = "akirak/batch-update-emacs-config-contents";
  };
}
