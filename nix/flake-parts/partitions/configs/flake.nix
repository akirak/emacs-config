{
  inputs = {
    flake-pins.url = "github:akirak/flake-pins";

    denix = {
      url = "github:yunfachi/denix";
      inputs.nixpkgs.follows = "flake-pins/unstable";
      inputs.home-manager.follows = "flake-pins/home-manager-unstable";
      # inputs.nix-darwin.follows = "flake-pins/nix-darwin";
    };
  };

  outputs = _: { };
}
