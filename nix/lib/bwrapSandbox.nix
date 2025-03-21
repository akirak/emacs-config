# Copyright (C) 2023-2025 Akira Komamura
# SPDX-License-Identifier: MIT
# From prev
{
  lib,
  writeText,
  coreutils,
  bubblewrap,
  writeShellScriptBin,
  gnused,
}:
with builtins;
let
  wrap =
    open: end: body:
    open + body + end;

  # lib.escapeShellArgs quotes each argument with single quotes. It is safe, but
  # I want to allow use of environment variables passed as arguments.
  quoteShellArgs = lib.concatMapStringsSep " " (wrap "\"" "\"");
in
{
  name ? "emacs",
  # Options related to the sandbox
  protectHome ? true,
  userEmacsDirectory ? null,
  userEmacsDirectoryMountPoint ? "\${XDG_CONFIG_HOME:-$HOME/.config}/emacs",
  shareNet ? false,
  inheritPath ? false,
  extraDirsToTryBind ? [ ],
  extraBwrapOptions ? [ ],
  emacsArguments ? [ ],
}:
emacs-env:
let
  arguments =
    (
      if protectHome then
        [
          "--dir"
          "$HOME"
          "--bind-try"
          "$HOME/.cache/fontconfig"
          "$HOME/.cache/fontconfig"
          "--ro-bind-try"
          "$HOME/.config/zsh/.zshenv"
          "$HOME/.config/zsh/.zshenv"
          "--ro-bind-try"
          "$HOME/.config/zsh/.zshrc"
          "$HOME/.config/zsh/.zshrc"
          "--ro-bind-try"
          "$HOME/.config/zsh/plugins"
          "$HOME/.config/zsh/plugins"
          "--ro-bind-try"
          "$HOME/.zshenv"
          "$HOME/.zshenv"
          "--ro-bind-try"
          "$HOME/.config/direnv"
          "$HOME/.config/direnv"
          "--ro-bind-try"
          "$HOME/.config/nix"
          "$HOME/.config/nix"
          "--ro-bind-try"
          "$HOME/.config/nixpkgs"
          "$HOME/.config/nixpkgs"
        ]
      else
        [
          "--bind"
          "$HOME"
          "$HOME"
        ]
    )
    ++ (
      if isString userEmacsDirectory then
        [
          "--bind"
          userEmacsDirectory
          userEmacsDirectoryMountPoint
        ]
      else
        [
          "--tmpfs"
          userEmacsDirectoryMountPoint
        ]
    )
    ++ [
      "--setenv"
      "PATH"
      (
        if inheritPath then
          "$PATH"
        else
          lib.makeBinPath [
            "$HOME/.nix-profile/bin"
            coreutils
            gnused
          ]
      )

      "--ro-bind"
      ../../early-init.el
      (userEmacsDirectoryMountPoint + "/early-init.el")

      "--ro-bind"
      (writeText "init.el" ''
        ;; -*- lexical-binding: t; no-byte-compile: t; -*-
        (dolist (file '(${quoteShellArgs emacs-env.initFiles}))
          (load file nil (not init-file-debug)))
      '')
      (userEmacsDirectoryMountPoint + "/init.el")
    ]
    ++ (concatLists (
      map (dir: [
        "--bind-try"
        "$(readlink -f ${dir})"
        dir
      ]) extraDirsToTryBind
    ))
    # Bind directories on non-NixOS systems
    ++ (concatLists (
      map
        (dir: [
          "--ro-bind-try"
          dir
          dir
        ])
        (
          [
            "/lib"
            "/lib32"
            "/lib64"
            "/libx32"
            "/libexec"
          ]
          ++ (
            if inheritPath then
              [
                "/usr/bin"
                "/usr/local/bin"
                "/usr/lib"
                "/usr/local/lib"
                "/usr/share"
                "/usr/local/share"
              ]
            else
              [ "/usr/share/fonts" ]
          )
        )
    ))
    ++ extraBwrapOptions
    ++ (lib.optional shareNet "--share-net")
    ++ [
      "${emacs-env}/bin/emacs"
    ]
    ++ emacsArguments;
in
writeShellScriptBin name ''
  cleanup() {
    rmdir "${userEmacsDirectoryMountPoint}"
  }

  if [[ ! -d "${userEmacsDirectoryMountPoint}" ]]
  then
    trap cleanup EXIT ERR
  fi

  if [[ -v XAUTHORITY ]]
  then
    xauthority_args="--ro-bind $XAUTHORITY $XAUTHORITY"
  else
    xauthority_args=""
  fi

  if [[ ''${DBUS_SESSION_BUS_ADDRESS} =~ ^unix:path=([^,]+) ]]
  then
    dbus_args="--ro-bind ''${BASH_REMATCH[1]} ''${BASH_REMATCH[1]} --setenv DBUS_SESSION_BUS_ADDRESS ''${DBUS_SESSION_BUS_ADDRESS}"
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
      --ro-bind /nix/store /nix/store \
      --bind /nix/var /nix/var \
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
      --ro-bind-try /tmp/.X11-unix/X0 /tmp/.X11-unix/X0 \
      ''${xauthority_args} \
      ''${dbus_args} \
      --unshare-all \
      ${quoteShellArgs arguments} "$@"
    )
''
