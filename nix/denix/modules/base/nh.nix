{
  delib,
  lib,
  pkgs,
  homeconfig,
  host,
  ...
}:
delib.module {
  name = "nh";

  options =
    let
      homeDirectory = homeconfig.home.homeDirectory;
    in
    with delib;
    moduleOptions {
      enable = boolOption host.isDesktop;

      rebuildScript = {
        enable = boolOption true;
      };

      mainConfigDirectory = strOption "${homeDirectory}/build/nix-config";

      emacsConfigDirectory = strOption "${homeDirectory}/build/emacs-config";

      cachixName = allowNull (strOption null);
    };

  home.ifEnabled =
    { cfg, ... }:
    let
      notify = pkgs.writeShellApplication {
        name = "notify";
        text = ''
          name="$1"
          shift
          if [[ -v DISPLAY ]] || [[ -v WAYLAND_DISPLAY ]]
          then
            ${lib.getExe pkgs.notify-desktop} -r "$name" -t 5000 "$@"
          else
            echo >&2 "$@"
          fi
        '';
      };

      setBuildFlags = ''
        if emacs_config="$(readlink -e "${cfg.emacsConfigDirectory}")"
        then
          build_flags=(--override-input emacs-config "''${emacs_config}")
        else
          build_flags=()
        fi
      '';

      rebuildScript = pkgs.writeShellScriptBin "nixos-rebuild-and-notify" ''
        flake="${cfg.mainConfigDirectory}"
        notify="${lib.getExe notify}"
        cachix="${if cfg.cachixName != null then cfg.cachixName else ""}"
        if [[ -z "$cachix" ]]; then
          unset cachix
        fi
        nh="${lib.getExe pkgs.nh}"

        command_exists() {
          command -v "$1" >/dev/null 2>/dev/null
        }

        usage() {
          cat >&2 <<HELP
          Usage: nixos-rebuild-and-notify TARGET [OPERATION]

          TARGET: "os" or "home"
          OPERATION: One of the subcommands of `nh os|home`
        HELP
        }

        if [[ $# -eq 0 ]]; then
          usage
          exit 1
        fi

        target="$1"
        shift
        operation="''${1:-switch}"
        if [[ $# -gt 0 ]]; then
          shift
        fi

        case "$target" in
          os)
            ;;
          home)
            ;;
          *)
            echo >&2 "Invalid target: $target"
            usage
            exit 1
        esac

        ${setBuildFlags}

        if [[ -v cachix ]] && [[ -n "$cachix" ]] && command_exists cachix; then
          command=(cachix watch-exec "$cachix" "$nh" --)
        else
          command=("$nh")
        fi

        if "''${command[@]}" "$target" "$operation" "$flake" \
          -- --option accept-flake-config true \
          ''${build_flags[@]} "''${@}"; then
            "$notify" rebuildScript "Rebuilding the configuration (nh $target $operation) has finished successfully"
        else
          "$notify" rebuildScript "Rebuilding the configuration (nh $target $operation) has failed"
          if [[ -v DISPLAY ]] || [[ -v WAYLAND_DISPLAY ]]
          then
            read
          fi
        fi
      '';

      updateScript = pkgs.writeShellApplication {
        name = "nh-update";
        runtimeInputs = [
          pkgs.nh
        ];
        text = ''
          cd "${cfg.mainConfigDirectory}"
          ${setBuildFlags}

          echo >&2 "Running nix flake update..."
          echo >&2 "  Directory: $PWD"
          echo >&2 "  Flags: ''${build_flags[*]}"
          nix flake update "''${build_flags[@]}"
        '';
      };
    in
    {
      programs.nh = {
        enable = true;
        clean = {
          enable = true;
          dates = "06,16,26-*-* 00:00:00";
          extraArgs = "--keep-since 5d --no-gcroots";
        };
        flake = cfg.mainConfigDirectory;
      };

      home.packages = lib.optionals cfg.rebuildScript.enable [
        rebuildScript
        updateScript
      ];
    };
}
