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
in
{
  options = {
    programs.emacs-twist.settings = {
      extraFeatures = lib.mkOption {
        type = types.listOf types.str;
        description = "List of options";
        default = [ ];
      };
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

    home.packages = with pkgs'; [
      copilot-language-server

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
