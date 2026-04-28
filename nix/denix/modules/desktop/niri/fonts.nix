{ delib, pkgs, ... }:
delib.module {
  name = "niri";

  home.ifEnabled =
    { ... }:
    {
      home.packages = with pkgs; [
        # fonts
        customFontPackages.jetbrains-mono-nerdfont
        # Japanese
        ipafont
      ];
    };
}
