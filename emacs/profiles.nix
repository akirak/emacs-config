/*
This is a function that takes an Emacs configuration defined in ./overlay.nix
and returns an attribute set of alternative configuration profiles.
*/
emacs-config: let
  personalInitPreamble = ''
    ;; -*- lexical-binding: t; no-byte-compile: t; -*-
    (setq custom-file (locate-user-emacs-file "custom.el"))
    (setq akirak/enabled-status-tags t)
  '';
in rec {
  /*
  Generally useful configuration for developer's work.
  */
  developer-pgtk = emacs-config.override (_: {
    pgtk = true;

    prependToInitFile = personalInitPreamble;

    extraFeatures = [
      "mermaid"
    ];
  });

  developer = developer-pgtk.override (_: {
    pgtk = false;
  });
}