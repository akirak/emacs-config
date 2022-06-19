{pkgs, ...}: {
  fonts = {
    fonts = with pkgs; [
      jetbrains-mono-nerdfont
      merriweather
      lato
    ];

    fontconfig.defaultFonts = {
      monospace = ["JetBrains Mono NF"];

      sansSerif = ["Lato"];

      serif = ["Merriweather"];
    };
  };

  hardware.pulseaudio.enable = true;
  nixpkgs.config.pulseaudio = true;

  systemd.services.setxkbmap = {
    enable = true;
    after = "post-resume.target";
    description = "Run setxkbmap";

    script = "/run/current-system/sw/bin/setxkbmap -option ctrl:nocaps";
    environment = {
      DISPLAY = ":0";
    };
  };
}
