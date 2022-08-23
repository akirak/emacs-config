{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf mkOption types;

  cfg = config.programs.emacs-unsafe;

  wrapper = pkgs.emacsSandboxed {
    name = "emacs-unsafe";
    withXwidgets = true;
    inherit
      (cfg)
      extraFeatures
      extraInitFiles
      extraInitText
      extraDirsToTryBind
      ;
    shareNet = true;
    protectHome = false;
    inheritPath = true;
    userEmacsDirectory = "$HOME/emacs";
  };
in {
  options = {
    programs.emacs-unsafe = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Install the Emacs configuration.
        '';
      };

      extraFeatures = mkOption {
        type = types.listOf types.string;
        default = [];
        description = ''
          List of extra features to enable.
        '';
      };

      extraInitFiles = mkOption {
        type = types.listOf types.path;
        default = [];
        description = ''
          List of extra init files to load.
        '';
      };

      extraInitText = mkOption {
        type = types.nullOr types.string;
        default = null;
        description = ''
          Extra contents appended to init.el.
        '';
      };

      extraDirsToTryBind = mkOption {
        type = types.listOf types.string;
        default = [];
        description = ''
          Extra directories to bind using --bind-try option of bubblewrap.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      wrapper
    ];
  };
}
