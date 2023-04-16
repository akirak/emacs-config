packages: {
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) types;

  cfg = config.programs.emacs-twist;
in {
  options = {
    programs.emacs-twist.settings = {
      extraFeatures = lib.mkOption {
        type = types.listOf types.str;
        description = "List of options";
        default = [];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs.emacs-twist = {
      emacsclient.enable = true;
      directory = ".local/share/emacs";
      earlyInitFile = ./early-init.el;
      createInitFile = true;
      config = packages.${pkgs.system}.emacs-config.override {
        inherit (cfg.settings) extraFeatures;
        prependToInitFile = ''
          ;; -*- lexical-binding: t; no-byte-compile: t; -*-
          (setq custom-file (locate-user-emacs-file "custom.el"))
          (setq akirak/enabled-status-tags t)
        '';
      };
      serviceIntegration.enable = true;
      createManifestFile = true;
    };

    home.packages = with pkgs; [
      # Font families used in my Emacs config
      cascadia-code
      inter
      source-han-sans
      noto-fonts-emoji
      symbola
    ];

    # Generate a desktop file for emacsclient
    services.emacs.client.enable = true;
  };
}
