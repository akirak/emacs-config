{
  defaultFeatures,
  lib,
}: emacs-config: let
  makeProfile = {
    presets,
    personalized,
    otherSettings,
  }:
    lib.nameValuePair
    "${presets.name}-${otherSettings.name}${
      if personalized
      then "-personalized"
      else ""
    }"
    (
      emacs-config.override (_:
        {
          extraFeatures = presets.value;

          prependToInitFile = ''
            ;; -*- lexical-binding: t; no-byte-compile: t; -*-
              (setq custom-file (locate-user-emacs-file "custom.el"))
              (setq akirak/enabled-status-tags ${
              if personalized
              then "t"
              else "nil"
            })
          '';
        }
        // otherSettings.value)
    );

  matrix = {
    presets = lib.mapAttrsToList lib.nameValuePair {
      default = defaultFeatures;
    };

    personalized = [
      true
      false
    ];

    otherSettings = lib.mapAttrsToList lib.nameValuePair {
      x11 = {
        pgtk = false;
      };
      pgtk = {
        pgtk = true;
      };
    };
  };

  go = acc: {
    key,
    values,
  }:
    lib.flatten
    (builtins.map (
        prev:
          builtins.map (value: prev // {${key} = value;}) values
      )
      acc);
in
  lib.pipe matrix [
    (lib.mapAttrsToList (key: values: {inherit key values;}))
    (builtins.foldl' go [{}])
    (builtins.map makeProfile)
    lib.listToAttrs
  ]
