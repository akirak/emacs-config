# From prev
{
  lib,
  emacs-config,
  writeText,
  coreutils,
  gnused,
}:
with builtins; let
  wrap = open: end: body: open + body + end;

  quoteShellArgs = lib.concatMapStringsSep " " (wrap "\"" "\"");
in
  # From final (self)
  {bubblewrapGUI}: {
    name ? "emacs",
    nativeCompileAheadDefault ? true,
    automaticNativeCompile ? true,
    enableOpinionatedSettings ? true,
    extraFeatures ? [],
    extraInitFiles ? [],
    extraInitText ? null,
    withXwidgets ? false,
    # Options related to the sandbox
    protectHome ? true,
    userEmacsDirectory ? null,
    userEmacsDirectoryMountPoint ? "\${XDG_CONFIG_HOME:-$HOME/.config}/emacs",
    shareNet ? false,
    inheritPath ? false,
    extraDirsToTryBind ? [],
    extraBwrapOptions ? [],
    emacsArguments ? [],
  }: let
    package = emacs-config.override {
      inherit extraFeatures extraInitFiles withXwidgets nativeCompileAheadDefault;
    };

    initEl = writeText "init.el" ''
      ;; -*- lexical-binding: t; no-byte-compile: t; -*-
      (setq custom-file (locate-user-emacs-file "custom.el"))
      ${lib.optionalString (!automaticNativeCompile) ''
        ;; Turn off automatic native compilation
        (setq inhibit-automatic-native-compilation t)
      ''}
      ${lib.optionalString enableOpinionatedSettings ''
        ;; Turn on settings inside :status clauses
        (setq akirak/enabled-status-tags t)
      ''}
      (dolist (file '(${quoteShellArgs package.initFiles}))
        (load file nil (not init-file-debug)))
      ${lib.optionalString (isString extraInitText) extraInitText}
    '';
  in
    bubblewrapGUI {
      inherit name;

      preamble = ''
        cleanup() {
          rmdir "${userEmacsDirectoryMountPoint}"
        }

        if [[ ! -d "${userEmacsDirectoryMountPoint}" ]]
        then
          trap cleanup EXIT ERR
        fi
      '';

      arguments =
        (
          if protectHome
          then [
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
          else [
            "--bind"
            "$HOME"
            "$HOME"
          ]
        )
        ++ (
          if isString userEmacsDirectory
          then [
            "--bind"
            userEmacsDirectory
            userEmacsDirectoryMountPoint
          ]
          else ["--tmpfs" userEmacsDirectoryMountPoint]
        )
        ++ [
          "--setenv"
          "PATH"
          (
            if inheritPath
            then "$PATH"
            else
              lib.makeBinPath [
                "$HOME/.nix-profile/bin"
                coreutils
                gnused
              ]
          )

          "--ro-bind"
          ../../../emacs/early-init.el
          (userEmacsDirectoryMountPoint + "/early-init.el")

          "--ro-bind"
          initEl
          (userEmacsDirectoryMountPoint + "/init.el")
        ]
        ++ (concatLists (
          map (dir: [
            "--bind-try"
            "$(readlink -f ${dir})"
            dir
          ])
          extraDirsToTryBind
        ))
        # Bind directories on non-NixOS systems
        ++ (concatLists (
          map (dir: [
            "--ro-bind-try"
            dir
            dir
          ])
          ([
              "/lib"
              "/lib32"
              "/lib64"
              "/libx32"
              "/libexec"
            ]
            ++ (
              if inheritPath
              then [
                "/usr/bin"
                "/usr/local/bin"
                "/usr/lib"
                "/usr/local/lib"
                "/usr/share"
                "/usr/local/share"
              ]
              else ["/usr/share/fonts"]
            ))
        ))
        ++ extraBwrapOptions
        ++ (lib.optional shareNet "--share-net")
        ++ [
          "${package}/bin/emacs"
        ]
        ++ emacsArguments;
    }
