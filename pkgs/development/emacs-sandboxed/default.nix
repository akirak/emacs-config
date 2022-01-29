# From prev
{ lib
, emacs-config
, writeText
, coreutils
, gnused
}:
with builtins;
let
  wrap = open: end: body: open + body + end;

  quoteShellArgs = lib.concatMapStringsSep " " (wrap "\"" "\"");
in
# From final (self)
{ bubblewrapGUI }:
{ name ? "emacs"
, enabledOpinionatedSettings ? true
, extraFeatures ? [ ]
, extraInitFiles ? [ ]
, extraInitText ? null
  # Options related to the sandbox
, protectHome ? true
, userEmacsDirectory ? null
, userEmacsDirectoryMountPoint ? "$HOME/.emacs.d"
, shareNet ? false
, inheritPath ? false
, extraDirsToTryBind ? [ ]
, extraBwrapOptions ? [ ]
}:
let
  package = emacs-config.override {
    inherit extraFeatures extraInitFiles;
  };

  initEl = writeText "init.el" ''
    ;; -*- lexical-binding: t; no-byte-compile: t; -*-
    (setq custom-file (locate-user-emacs-file "custom.el"))
    ${lib.optionalString enabledOpinionatedSettings ''
      ;; Turn on settings inside :status clauses
      (setq akirak/enabled-status-tags t)
    ''}
    (dolist (file '(${quoteShellArgs package.initFiles}))
      (load file t t))
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
    (if protectHome
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
    ])
    ++
    (if isString userEmacsDirectory
    then [
      "--bind"
      userEmacsDirectory
      userEmacsDirectoryMountPoint
    ]
    else [ "--tmpfs" "$HOME/.emacs.d" ]
    )
    ++
    [
      "--setenv"
      "PATH"
      (if inheritPath
      then "$PATH"
      else
        lib.makeBinPath [
          coreutils
          gnused
        ])

      "--ro-bind"
      ../../../emacs/early-init.el
      (userEmacsDirectoryMountPoint + "/early-init.el")

      "--ro-bind"
      initEl
      (userEmacsDirectoryMountPoint + "/init.el")
    ] ++
    (concatLists (map (dir: [ "--bind-try" dir dir ])
      extraDirsToTryBind
    ))
    ++
    extraBwrapOptions ++
    (lib.optional shareNet "--share-net")
    ++ [
      "${package}/bin/emacs"
    ];
}
