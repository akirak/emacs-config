{
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        height = 30;
        spacing = 4;

        "modules-left" = [
          # "river/tags"
        ];

        "modules-center" = [
          # "river/mode"
          "river/window"
        ];

        "modules-right" = [
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          # "backlight"
          "keyboard-state"
          # "sway/language"
          "battery"
          "battery#bat2"
          "clock"
          "tray"
        ];

        "keyboard-state" = {
          numlock = true;
          capslock = true;
          format = "{name} {icon}";
          "format-icons" = {
            locked = "";
            unlocked = "";
          };
        };

        pulseaudio = {
          format = "{format_source} {volume}% {icon}";
        };

        clock = {
          format = "{:%Y-%m-%d (%a) W%W %H:%M}";
        };
      };
    };
  };
}
