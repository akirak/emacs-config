{ config, pkgs, lib, ... }:
let
  inherit (lib) mkIf mkOption types;

  cfg = config.programs.emacs-config-old;

  wrapper = pkgs.writeShellScriptBin "emacs-old" ''
    nix run --no-update-lock-file \
      ${config.xdg.configHome}/emacs -- \
        --eval "(progn
          (setq custom-file \"${config.xdg.dataHome}/emacs/custom.el\")
          (when (file-exists-p custom-file)
            (load custom-file nil :nomessage)))"
  '';
in
{
  options = {
    programs.emacs-config-old = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          If enabled, install a desktop file for running the old Emacs config.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      wrapper
      (pkgs.makeDesktopItem {
        name = "Emacs Config (2018)";
        type = "Application";
        desktopName = "emacs-config-2018";
        # custom-file is loaded inside my init.el
        exec = "${wrapper}/bin/emacs-old";
      })
    ];
  };
}
