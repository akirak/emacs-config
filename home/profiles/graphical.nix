{pkgs, ...}: {
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

  programs.mpv = {
    enable = true;
  };
}
