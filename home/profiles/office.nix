{pkgs, ...}: {
  home.packages = with pkgs; [
    # Fail to build
    # wpsoffice
    ipafont
  ];
}
