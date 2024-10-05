{
  inputs = {
    git-hooks-nix.url = "github:cachix/git-hooks.nix";

    flake-no-path = {
      url = "github:akirak/flake-no-path";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "git-hooks-nix";
    };
  };

  outputs = { ... }: { };
}
