{pkgs, ...}: {
  home.packages = with pkgs; [
    blanket

    # Fonts
    cascadia-code
    aileron
    source-han-sans
  ];

  programs.alacritty = {
    # Not on Wayland
    enable = false;
  };

  programs.mpv = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
  };
}
