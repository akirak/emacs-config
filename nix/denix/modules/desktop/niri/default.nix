{
  delib,
  lib,
  pkgs,
  ...
}:
delib.module {
  name = "niri";

  options.niri = with delib; {
    enable = boolOption false;

    include = listOfOption str [ ];

    inputConfig = pathOption ./etc/input.kdl;

    outputConfig = pathOption ./etc/output.kdl;

    layoutConfig = pathOption ./etc/layout.kdl;

    animationsConfig = pathOption ./etc/animations.kdl;

    # I am not using spawn-at-startup or spawn-sh-at-startup at the top-level.

    skipHotkeyOverlayAtStartup = boolOption true;

    # Not using prefer-no-csd either.

    # Set to null to disable screenshot.
    screenshotPath = strOption "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

    # https://yalter.github.io/niri/Configuration:-Window-Rules
    # Allow adding host-specific window rules by appending them to this list.
    defaultWindowRules = listOfOption str [
      ''
        window-rule {
          match app-id=r#"^foot$"#
          default-column-width { proportion 0.33; }
        }
      ''
      ''
        window-rule {
          match app-id=r#"^mpv$"#

          open-floating true
          max-width 1200
          max-height 800
        }
      ''
      ''
        window-rule {
          match title=r#"^Rebuilding NixOS"#

          open-focused false
        }
      ''
    ];

    extraWindowRules = listOfOption str [ ];

    bindsConfig = pathOption ./etc/binds.kdl;
  };

  nixos.ifEnabled = {
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    programs.niri = {
      enable = true;
    };

    environment.systemPackages = [
      # Enables XWayland. See https://niri-wm.github.io/niri/Xwayland.html
      pkgs.xwayland-satellite
    ];

    environment.etc."wayland-sessions/Niri.desktop".text = ''
      [Desktop Entry]
      Name=Niri
      Exec=niri-session
      Type=Application
    '';
  };

  home.ifEnabled =
    { cfg, ... }:
    {
      xdg.configFile."niri/config.kdl".source =
        pkgs.runCommand "config.kdl"
          {
            includeKdl = lib.concatStringsSep "\n" cfg.include;

            windowRules = lib.concatStringsSep "\n" (cfg.defaultWindowRules ++ cfg.extraWindowRules);

            hotkeyOverlay = ''
              hotkey-overlay {
                ${if cfg.skipHotkeyOverlayAtStartup then "skip-at-startup" else ""}
              }
            '';

            screenshotPath = lib.optionalString (cfg.screenshotPath != null) ''
              screenshot-path "${cfg.screenshotPath}"
            '';

            passAsFile = [
              "includeKdl"
              "hotkeyOverlay"
              "windowRules"
              "screenshotPath"
            ];
          }
          ''
            tmp=$(mktemp)

            # include can cause a validation error if validation is performed
            # in a temporary environment, so exclude it from validation.
            cat > $tmp \
              ${cfg.inputConfig} \
              ${cfg.outputConfig} \
              ${cfg.layoutConfig} \
              $hotkeyOverlayPath \
              $screenshotPathPath \
              ${cfg.animationsConfig} \
              $windowRulesPath \
              ${cfg.bindsConfig}

            ${lib.getExe pkgs.niri} validate -c "$tmp"

            cat "$includeKdlPath" "$tmp" > $out
          '';

      # Requires the home module from the xremap flake
      services.xremap.withNiri = true;
    };
}
