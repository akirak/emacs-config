{ pkgs
, emacsBinaryPackage
}:
let
  emacsConfig = { name, stages, funcName }: {
    enable = true;
    inherit stages;
    inherit name;
    entry = builtins.concatStringsSep " " [
      "${pkgs.emacsProfiles.batch}/bin/emacs"
      "--batch -l ${./scripts/update-emacs-config.el}"
      "-f ${funcName}"
    ];
    files = "emacs-config\.org$";
    pass_filenames = true;
  };

  pushEmacsBinary = pkgs.writeShellScript "push-emacs-binary" ''
    result=$(timeout 3 nix eval --raw .#${emacsBinaryPackage}) \
    && timeout 5 cachix push akirak "$result"
  '';
in
{
  nixpkgs-fmt.enable = true;

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
