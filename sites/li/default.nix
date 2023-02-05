let
  username = "akirakomamura";
in {
  hostName = "li";
  inherit username;

  homeModules = [
    "river"
    "symlinks"
    "personal"
    "work"
  ];

  extraHomeModules = [
    ({
      pkgs,
      site,
      ...
    }: {
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

      programs.emacs-twist = {
        enable = true;
        emacsclient.enable = true;
        directory = "emacs";
        earlyInitFile = ../../emacs/early-init.el;
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
    })
  ];

  nixos = {
    users.users = {
      hashedPassword = "$6$3LmgpFGu4WEeoTss$9NQpF4CEO8ivu0uJTlDYXdiB6ZPHBsLXDZr.6S59bBNxmNuhirmcOmHTwhccdgSwq7sJOz2JbOOzmOCivxdak0";
    };
  };
}
