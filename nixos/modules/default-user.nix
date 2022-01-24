{ site, homeModules, pkgs, ... } @ args:
{
  users.users.${site.username} = {
    uid = 1000;
    description = "default";
    isNormalUser = true;
  } // site.nixos.users.users;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${site.username} = {
      imports = site.homeModules;
    };
  };
}
