{pkgs, ...}: {
  imports = [
    ../../foot.nix
    ./dunst.nix
    ./waybar.nix
    ./kanshi.nix
  ];

  home.packages = with pkgs; [
    # riverctl and rivertile need to be in PATH
    river

    (pkgs.writeShellScriptBin "river-session" ''
      export XKB_DEFAULT_LAYOUT=us
      export XKB_DEFAULT_OPTIONS=ctrl:nocaps
      export XDG_SESSION_TYPE=wayland
      export XDG_SESSION_DESKTOP=sway
      export XDG_CURRENT_DESKTOP=sway
      export MOZ_ENABLE_WAYLAND=1
      # Set SWAYSOCK to use swaymsg and other sway-compatible utilities
      export SWAYSOCK=''${XDG_RUNTIME_DIR}/wayland-1
      exec ${pkgs.dbus}/bin/dbus-run-session -- river
    '')

    wofi
    (pkgs.writeShellApplication {
      name = "lock-screen";
      runtimeInputs = [pkgs.swaylock-effects];
      # TODO: Use a color scheme
      text = ''
        swaylock -f --clock --fade-in 0.5
      '';
    })

    (pkgs.callPackage ./rebuild.nix {})
    wayshot
    wf-recorder
    slurp # Used with wayshot

    pavucontrol
  ];

  xdg.configFile."river/init".source = ./init;
}
