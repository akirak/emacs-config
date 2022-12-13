{
  config,
  pkgs,
  ...
}: {
  services.dunst = {
    enable = true;
    waylandDisplay = "wayland-1";
    settings = {
      global = {
        # monitor = 0;
        width = 300;
        height = 300;
        origin = "top-right";
        offset = "50x65";

        padding = 16;
        horizontal_padding = 16;

        transparency = 10;

        font = "JetBrainsMono Nerd Font 10";
        line_height = 4;
        format = ''<b>%s</b>\n%b'';
      };
    };
  };
}
