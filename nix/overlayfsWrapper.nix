{
  fuse-overlayfs,
  writeShellScriptBin,
}: name: emacs-env:
writeShellScriptBin name ''
  set +u
  set -x

  runtimedir="''${XDG_RUNTIME_DIR:-$TMPDIR}"
  initdir="$runtimedir/${name}/emacs.d"
  workdir="$runtimedir/${name}/emacs.d-work"

  datadir="''${XDG_DATA_HOME:-$HOME/.local/share}"
  upperdir="$datadir/${name}"
  lowerdir="$datadir/emacs"

  [[ -d "$lowerdir" ]]

  mkdir -p "$initdir" "$workdir" "$upperdir"

  ${fuse-overlayfs}/bin/fuse-overlayfs \
    -o upperdir="$upperdir" \
    -o workdir="$workdir" \
    -o lowerdir="$lowerdir" \
    "$initdir"

  cleanup() {
    fusermount3 -u "$initdir"
  }

  trap cleanup ERR EXIT

  ${emacs-env}/bin/emacs --init-directory="$initdir" "$@"
''
