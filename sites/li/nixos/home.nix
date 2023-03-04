{
  homeUser,
  pkgs,
  ...
}: let
  mainMonitor = {
    criteria = "Unknown VA32AQ K3LMAS000141 (HDMI-A-2)";
    mode = "2560x1440";
    position = "0,0";
  };

  subMonitor = {
    criteria = "Dell Inc. DELL S2421HS CBPT223 (DP-1)";
    mode = "1920x1080";
    position = "2560,100";
  };
in {
  environment.systemPackages = [
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDicts.en_GB-ise
  ];

  home-manager.users.${homeUser} = {
    programs.chromium = {
      enable = true;
      extensions = [
        {
          # Google Input Tools
          id = "mclkkofklkfljcocdinagocijmpgbhab";
        }
      ];
    };

    home.packages = [
      pkgs.wine
      pkgs.tenacity
    ];

    services.kanshi.profiles = {
      docked.outputs = [mainMonitor subMonitor];
      undocked.outputs = [mainMonitor];
    };

    programs.river.enable = true;

    programs.gpg.enable = true;

    programs.emacs-twist = {
      enable = true;
      emacsclient.enable = true;
      directory = "emacs";
      earlyInitFile = ../../../emacs/early-init.el;
      config = pkgs.emacs-config.override {
        extraFeatures = [
          # "beancount"
          "mermaid"
          # "ChatGPT"
          "copilot"
          "OCaml"
          # "Lean4"
          # "lsp_mode"
        ];
        prependToInitFile = ''
          ;; -*- lexical-binding: t; no-byte-compile: t; -*-
          (setq custom-file (locate-user-emacs-file "custom.el"))
          (setq akirak/enabled-status-tags t)
        '';
      };
    };
  };
}
