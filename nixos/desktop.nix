{
  imports = [
    ./base.nix
    ./baremetal.nix
    ./profiles/graphical.nix
    ./profiles/yubikey.nix
  ];

  nix = {
    gc = {
      dates = "2weeks";
      automatic = true;
    };
    optimise.automatic = false;

    settings = {
      sandbox = true;
      trusted-users = [ "root" "@wheel" ];
      allowed-users = [ "@wheel" ];
    };

    extraOptions = ''
      min-free = 536870912
      keep-outputs = true
      keep-derivations = true
      fallback = true
    '';
  };

  # Allow mounting FUSE filesystems as a user.
  # https://discourse.nixos.org/t/fusermount-systemd-service-in-home-manager/5157
  environment.etc."fuse.conf".text = ''
    user_allow_other
  '';

  # Necessary if you want to turn on allowOther in impermanence
  # https://github.com/nix-community/impermanence
  # programs.fuse.userAllowOther = true;

  security.sudo.wheelNeedsPassword = false;

  services.earlyoom.enable = true;

  services.psd.enable = true;
}
