{
  site,
  homeModules,
  pkgs,
  ...
} @ args: {
  users.defaultUserShell = pkgs.zsh;

  users.users.${site.username} =
    {
      uid = 1000;
      description = "default";
      isNormalUser = true;

      extraGroups = [
        "wheel"
        "video"
        "audio"
        "disk"
        "networkmanager"
        "systemd-journal"
      ];
    }
    // site.nixos.users.users;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${site.username} = {
      imports = site.homeModules;
    };
  };
}
