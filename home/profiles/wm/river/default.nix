{pkgs, ...}: {
  home.packages = with pkgs; [
    river
    wofi
  ];

  # home.file.".xsession-river".text = ''
  #   exec river
  # '';

  xdg.configFile."river/init".source = ./init;
}
