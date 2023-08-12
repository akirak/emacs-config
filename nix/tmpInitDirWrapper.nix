{
  writeShellScriptBin,
  runCommandLocal,
}: name: emacs-env: let
  initFile = runCommandLocal "init.el" {} ''
    mkdir -p $out
    touch $out/init.el
    for file in ${builtins.concatStringsSep " " emacs-env.initFiles}
    do
      cat "$file" >> $out/init.el
      echo >> $out/init.el
    done
  '';
in
  writeShellScriptBin name ''
    set +u
    set -x

    initdir="$(mktemp --tmpdir -d \"${name}-XXX\")"

    cleanup() {
      rm -rf "$initdir"
    }

    trap cleanup ERR EXIT

    ln -s ${initFile}/init.el "$initdir/init.el"
    ln -s ${../emacs/early-init.el} "$initdir/early-init.el"

    ${emacs-env}/bin/emacs --init-directory="$initdir" "$@"
  ''
