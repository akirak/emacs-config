# Based on https://github.com/yunfachi/nix-config/tree/9ba35d6fc96a4eb86db72c91a0fc74e636c71f82/modules/toplevel/home-manager.nix
{ delib,
  config,
  moduleSystem,
  homeManagerUser,
  ...
}:
delib.module {
  name = "home-manager";

  myconfig.always.args.shared.homeconfig =
    if moduleSystem == "home"
    then config
    else config.home-manager.users.${homeManagerUser};

  home.always = let
    username = "akirakomamura";
  in {
    home = {
      inherit username;
      homeDirectory = "/home/${username}";
    };

    nixpkgs.config.allowUnfree = true;
  };
}
