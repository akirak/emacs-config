{ lib
, bubblewrap
, writeShellScriptBin
, writeText
, coreutils
}:
emacs:
{ name ? "emacs-sandboxed"
, userEmacsDirectory ? null
, extraBubblewrapOptions ? [ ]
, emacsArguments ? [ ]
}:
let
  load = file: "(load \"${file}\" nil t)\n";

  initEl = writeText "init.el" (
    '';; -*- lexical-binding: t; no-byte-compile: t; -*-
      (setq custom-file (locate-user-emacs-file "custom.el"))
      (when (file-exists-p custom-file)
        (load custom-file nil t))
    ''
    +
    lib.concatMapStrings load emacs.initFiles
  );

  emacsDirectoryOpts =
    if userEmacsDirectory == null
    then "--dir $HOME/.emacs.d"
    # user-emacs-directory may contain references to paths inside itself, so
    # it is better to create a symlink rather than to bind-mount it.
    else ''
      --bind ${userEmacsDirectory} ${userEmacsDirectory} \
      --symlink ${userEmacsDirectory} $HOME/.emacs.d \
    '';

  userEmacsDirectory' =
    if userEmacsDirectory == null
    then "$HOME/.emacs.d"
    else userEmacsDirectory;
in
lib.extendDerivation true
{
  passthru.exePath = "/bin/${name}";
}
  (writeShellScriptBin name ''
    # If the first argument is an existing path, bind-mount it
    if [[ $# -gt 0 && -e "$1" ]]
    then
      f="$1"
      opts="--bind $f $f"
    else
      opts=""
    fi

    set -x
    ( exec ${bubblewrap}/bin/bwrap \
        --dir "$HOME" \
        --proc /proc \
        --dev /dev \
        --dev-bind-try /dev/snd /dev/snd \
        --dev-bind-try /dev/video0 /dev/video0 \
        --dev-bind-try /dev/video1 /dev/video1 \
        --dev-bind /dev/dri /dev/dri \
        --ro-bind /nix /nix \
        --ro-bind /etc /etc \
        --bind-try "$HOME/.cache/fontconfig" "$HOME/.cache/fontconfig" \
        --tmpfs /run \
        --tmpfs /tmp \
        --setenv DISPLAY ":0" \
        --ro-bind /tmp/.X11-unix/X0 /tmp/.X11-unix/X0 \
        --ro-bind "$XAUTHORITY" "$XAUTHORITY" \
        --new-session \
        --die-with-parent \
        --unshare-all \
        --setenv PATH ${lib.makeBinPath [ coreutils ]} \
        ${lib.escapeShellArgs extraBubblewrapOptions} \
        ${emacsDirectoryOpts} \
        --ro-bind ${../emacs/early-init.el} ${userEmacsDirectory'}/early-init.el \
        --ro-bind ${initEl} ${userEmacsDirectory'}/init.el \
        $opts \
        ${emacs}/bin/emacs ${lib.escapeShellArgs emacsArguments} "$@"
      )
  '')
