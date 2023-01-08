{
  emacs-config,
  writeShellApplication,
}: let
  inherit (builtins) head;

  emacs = emacs-config;

  initFile = head emacs-config.initFiles;
in
  writeShellApplication {
    name = "emacs-root";
    runtimeInputs = [
      emacs
    ];
    text = ''
      sandbox=$(mktemp -d -t emacs-rootXXX)
      cleanup() {
        echo "Cleaning up"
        rm -rf "$sandbox"
      }
      trap cleanup EXIT ERR
      ln -s ${initFile} "$sandbox/init.el"
      cp ${../../../emacs/early-init.el} "$sandbox/early-init.el"
      emacs --eval "(setq akirak/enabled-status-tags '(opinionated))" \
        --init-directory="$sandbox" "$@"
    '';
  }
