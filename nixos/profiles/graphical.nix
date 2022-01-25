{ pkgs, ... }:
{
  fonts = {
    fonts = with pkgs; [
      jetbrains-mono-nerdfont
      merriweather
      lato
    ];

    fontconfig.defaultFonts = {
      monospace = [ "JetBrains Mono NF" ];

      sansSerif = [ "Lato" ];

      serif = [ "Merriweather" ];
    };
  };

  hardware.pulseaudio.enable = true;
  nixpkgs.config.pulseaudio = true;
}
