{pkgs, ...}: {
  home.packages = with pkgs; [
    wpsoffice
    ipafont
  ];
}
