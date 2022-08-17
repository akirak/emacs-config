{pkgs, ...}: {
  home.packages = with pkgs; [
    blanket

    # Fonts
    cascadia-code
    libre-baskerville
    shippori-mincho
    dejavu_fonts
  ];

  programs.alacritty = {
    enable = true;
  };

  programs.mpv = {
    enable = true;
  };

  programs.firefox = {
    enable = false;
  };
}
