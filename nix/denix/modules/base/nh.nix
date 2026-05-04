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

      enableRebuildScript = boolOption true;

      mainConfigDirectory = strOption "${homeDirectory}/build/nix-config";

      emacsConfigDirectory = strOption "${homeDirectory}/build/emacs-config";

      cachixName = strOption null;
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

      rebuildScript = pkgs.writeShellScriptBin "nixos-rebuild-and-notify" ''
        flake="${cfg.mainConfigDirectory}"
        notify="${notify}"

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
        shift

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

        if emacs_config="$(readlink -e "${cfg.emacsConfigDirectory}")"
        then
          build_flags=(--override-input emacs-config "''${emacs_config}")
        else
          build_flags=()
        fi

        if json=$(${lib.getExe pkgs.nh} "$target" "$operation" --json "$flake" -- ''${build_flags[@]} "''${@}"); then
          "$notify" rebuildScript "Rebuilding the configuration (nh $target $operation) has finished successfully"

          out=$(echo "$json" | jq .outputs.out -)
          if command -v cachix >& /dev/null; then
            if cachix push akirak "$out"; then
              "$notify" rebuildScript "Successfully pushed the result to cachix."
            else
              "$notify" rebuildScript "Failed to push the result to cachix."
            fi
          fi
        else
          "$notify" rebuildScript "Rebuilding the configuration (nh $target $operation) has failed"
          if [[ -v DISPLAY ]] || [[ -v WAYLAND_DISPLAY ]]
          then
            read
          fi
        fi
      '';
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

      home.packages = lib.optional cfg.enableRebuildScript rebuildScript;
    };
}
