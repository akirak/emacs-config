{ pkgs, ... }:
{
  home.packages = with pkgs; [
    blanket

    # Fonts
    cascadia-code
    libre-baskerville
    shippori-mincho
  ];

  programs.alacritty = {
    enable = true;
  };

  programs.firefox = {
    # enable = true;
    # TODO: Add a package for the gnome extension
    # enableGnomeExtensions = true;
  };

  programs.chromium = {
    enable = false;
    package = pkgs.ungoogled-chromium;
  };

  programs.mpv = {
    enable = true;
  };

}
