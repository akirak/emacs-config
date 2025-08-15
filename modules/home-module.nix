# Copyright (C) 2023-2025 Akira Komamura
# SPDX-License-Identifier: MIT

makeConfig:
# Provide nixpkgs overlay from this config repository
{ overlays }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) types;

  cfg = config.programs.emacs-twist;

  pkgs' = pkgs.extend (lib.composeManyExtensions overlays);

  java-debug-plugin =
    pkgs.runCommand "java-debug-plugin.jar"
      {
        propagatedBuildInputs = [
          pkgs.vscode-extensions.vscjava.vscode-java-debug
        ];
      }
      ''
        jar=$(find ${pkgs.vscode-extensions.vscjava.vscode-java-debug}/share/vscode/extensions/vscjava.vscode-java-debug/server -name "*.jar")

        ln -s "$jar" $out
      '';
in
{
  options = {
    programs.emacs-twist.settings = {
      extraFeatures = lib.mkOption {
        type = types.listOf types.str;
        description = "List of options";
        default = [ ];
      };

      enableJava = lib.mkEnableOption "Enable enhanced Java support";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.emacs-twist = {
      emacsclient.enable = true;
      directory = ".local/share/emacs";
      earlyInitFile = ../early-init.el;
      createInitFile = true;
      config = makeConfig {
        pkgs = pkgs';
        features = cfg.settings.extraFeatures;
        prependToInitFile = ''
          ;; -*- lexical-binding: t; no-byte-compile: t; -*-
          (setq custom-file (locate-user-emacs-file "custom.el"))
          (setq akirak/enabled-status-tags t)
        '';
      };
      serviceIntegration.enable = lib.mkDefault true;
      createManifestFile = true;
    };

    home.file.${config.programs.emacs-twist.directory + "/ai-model-list.txt"}.source =
      ../ai-model-list.txt;

    home.file.${cfg.directory + "/java-debug-plugin.jar"} = lib.mkIf cfg.settings.enableJava {
      source = java-debug-plugin;
    };

    home.packages = with pkgs'; [
      # Font families used in my Emacs config
      cascadia-code
      inter
      source-han-sans
      noto-fonts-emoji
      symbola
    ];

    # Generate a desktop file for emacsclient
    services.emacs.client.enable = cfg.serviceIntegration.enable;
  };
}
