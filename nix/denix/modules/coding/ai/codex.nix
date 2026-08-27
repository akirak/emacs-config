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
    let
      hook = submodule {
        options = {
          type = noNullDefault (
            enumOption [
              "mcp_tool"
              "command"
            ] null
          );
          server = allowNull (strOption null);
          timeout = allowNull (intOption null);
          statusMessage = allowNull (strOption null);
          # Tool-specific
          tool = allowNull (strOption null);
          input = allowNull (attrsOption null);
          # Command-specific
          command = noNullDefault (strOption null);
          # Nix doesn't support Windows
          # commandWindows = strOption null;
          additionalContextLimit = allowNull (intOption null);
          async = boolOption false;
          cwd = allowNull (strOption null);
        };
      };

      hooksOption = listOfOption (submodule {
        options = {
          matcher = allowNull (strOption null);
          hooks = listOfOption hook [ ];
        };
      }) [ ];
    in
    moduleOptions {
      enable = boolOption host.codingFeatured;

      sqliteOnRuntimeDir = boolOption true;

      hooks = {
        PreToolUse = hooksOption;
        PermissionRequest = hooksOption;
        PostToolUse = hooksOption;
        PreCompact = hooksOption;
        PostCompact = hooksOption;
        UserPromptSubmit = hooksOption;
        SubagentStop = hooksOption;
        Stop = hooksOption;
        SessionStart = hooksOption;
        SubagentStart = hooksOption;
        SessionEnd = hooksOption;
      };
    };

  home.ifEnabled = { cfg, ... }: {
    home.packages = [
      pkgs.ai-tools.codex
    ];

    home.file = {
      ".codex/AGENTS.md".source = ./etc/AGENTS.md;

      ".codex/hooks.json".source = pkgs.writers.writeJSON "hooks.json" {
        hooks = lib.pipe cfg.hooks [
          (lib.filterAttrs (_: entries: builtins.length entries != 0))
          (builtins.mapAttrs (
            _: entries:
            builtins.map (entry: {
              inherit (entry) matcher;
              hooks = builtins.map (lib.filterAttrs (_: value: value != null)) entry.hooks;
            }) entries
          ))
        ];
      };
    };

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
