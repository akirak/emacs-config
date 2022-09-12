{pkgs, ...}: {
  home.packages = with pkgs; [
    blanket

    # Fonts
    cascadia-code
    aileron
    source-han-sans
  ];

  programs.alacritty = {
    enable = true;
  };

  programs.mpv = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
  };
}
