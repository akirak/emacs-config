{
  delib,
  inputs,
  host,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "emacs";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.isDesktop;

      # Assume the overlay is applied to pkgs
      package = packageOption pkgs.emacs-git-pgtk;

      extraFeatures = listOfOption str [ ];

      enableDaemon = boolOption true;
    };

  home.always.imports = [
    # Provides programs.emacs-twist
    inputs.twist.homeModules.emacs-twist
  ];

  home.ifEnabled =
    { cfg, myconfig, ... }:
    {
      programs.emacs-twist = {
        enable = true;
        emacsclient.enable = true;
        directory = ".local/share/emacs";
        earlyInitFile = ../../../../early-init.el;
        createInitFile = true;
        config = pkgs.emacs-env.override {
          inherit (cfg) extraFeatures;
          emacsPackage = cfg.package;
          prependToInitFile = ''
            ;; -*- lexical-binding: t; no-byte-compile: t; -*-
            (setq custom-file (locate-user-emacs-file "custom.el"))
            (setq akirak/enabled-status-tags t)
          '';
        };
        serviceIntegration.enable = cfg.enableDaemon;
        createManifestFile = true;
      };

      # Generate a desktop file for emacsclient
      services.emacs.client.enable = cfg.enableDaemon;

      systemd.user.services.emacs = lib.mkIf myconfig.wayland.enable {
        Service = {
          Environment = [
            "MOZ_ENABLE_WAYLAND=1"
            "WAYLAND_DISPLAY=${myconfig.wayland.display}"
          ];
        };
      };
    };
}
