{
  config,
  lib,
  ...
}: let
  inherit (lib) mkIf mkOption types;

  cfg = config.targets.crostini;
in {
  options = {
    targets.crostini = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable settings that make Home Manager work better on
          Crostini (Linux container) on Chrome OS.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile."systemd/user/cros-garcon.service.d/override.conf".text = ''
      [Service]
      Environment="PATH=${builtins.concatStringsSep ":"
        [
          "%h/.nix-profile/bin"
          "/usr/local/sbin"
          "/usr/local/bin"
          "/usr/local/games"
          "/usr/sbin"
          "/usr/bin"
          "/usr/games"
          "/sbin"
          "/bin"
        ]}" "XDG_DATA_DIRS=${builtins.concatStringsSep ":"
        [
          "%h/.nix-profile/share"
          "%h/.local/share"
          "/usr/local/share"
          "/usr/share"
        ]}"
    '';
  };
}
