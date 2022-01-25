{
  imports = [
    ./profiles/non-desktop-environment.nix
    ./profiles/display-managers/gdm.nix
  ];

  services.xserver.displayManager = {
    defaultSession = "xmonad";
    # You need to generate ~/.xsession using a home-manager
    session = [
      {
        manage = "desktop";
        name = "xmonad";
        start = ''
          exec $HOME/.xsession
        '';
      }
    ];
  };
}
