{
  lib,
  bubblewrap,
  writeShellScriptBin,
}: {
  name,
  arguments,
  preamble ? null,
}: let
  wrap = open: end: body: open + body + end;

  # lib.escapeShellArgs quotes each argument with single quotes. It is safe, but
  # I want to allow use of environment variables passed as arguments.
  quoteShellArgs = lib.concatMapStringsSep " " (wrap "\"" "\"");
in
  writeShellScriptBin name ''
    ${lib.optionalString (preamble != null) preamble}

    if [[ -v XAUTHORITY ]]
    then
      xauthority_args="--ro-bind $XAUTHORITY $XAUTHORITY"
    else
      xauthority_args=""
    fi

    if [[ ''${DBUS_SESSION_BUS_ADDRESS} =~ ^unix:path=([^,]+) ]]
    then
      dbus_args="--ro-bind ''${BASH_REMATCH[1]} ''${BASH_REMATCH[1]}"
    else
      dbus_args=""
    fi

    set -x
    ( exec ${bubblewrap}/bin/bwrap \
        --proc /proc \
        --dev /dev \
        --dev-bind-try /dev/snd /dev/snd \
        --dev-bind-try /dev/video0 /dev/video0 \
        --dev-bind-try /dev/video1 /dev/video1 \
        --dev-bind /dev/dri /dev/dri \
        --ro-bind /nix /nix \
        --ro-bind /etc /etc \
        --ro-bind-try /bin /bin \
        --ro-bind-try /usr/bin/env /usr/bin/env \
        --tmpfs /run \
        --ro-bind-try /run/current-system/sw/bin /run/current-system/sw/bin \
        --ro-bind-try /run/current-system/sw/etc /run/current-system/sw/etc \
        --ro-bind-try /run/current-system/sw/lib /run/current-system/sw/lib \
        --ro-bind-try /run/current-system/sw/share /run/current-system/sw/share \
        --ro-bind-try /run/opengl-driver /run/opengl-driver \
        --tmpfs /tmp \
        --setenv DISPLAY ":0" \
        --ro-bind /tmp/.X11-unix/X0 /tmp/.X11-unix/X0 \
        ''${xauthority_args} \
        ''${dbus_args} \
        --unshare-all \
        ${quoteShellArgs arguments} "$@"
      )
  ''
