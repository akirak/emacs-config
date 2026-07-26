{
  delib,
  pkgs,
  lib,
  host,
  ...
}:
delib.module {
  name = "codex";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;

      sqliteOnRuntimeDir = boolOption true;
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      pkgs.ai-tools.codex
    ];

    home.file.".codex/AGENTS.md".source = ./etc/AGENTS.md;

    systemd.user.services = lib.optionalAttrs cfg.sqliteOnRuntimeDir {
      codex-config = {
        Unit = {
          Description = "Tweak the configuration of codex";
        };

        Install = {
          WantedBy = [ "default.target" ];
        };

        Service = {
          Type = "oneshot";
          ExecStart = pkgs.writeShellScript "codex-config" ''
            set -euo pipefail

            configFile="$HOME/.codex/config.toml"
            content="sqlite_home = \"''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/codex\""

            if [[ -f "$configFile" ]]; then
              if ! grep -E "^sqlite_home" "$configFile" >/dev/null; then
                sed -i "1i $content" "$configFile"
                echo >&2 "Inserted a line into the beginning of the file."
              else
                echo >&2 "There is an already existing line, so skipped overwriting."
              fi
            else
              echo "$content" > "$configFile"
              echo >&2 "Created a new file."
            fi
          '';
        };
      };
    };
  };
}
