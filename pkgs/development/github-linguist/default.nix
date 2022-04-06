{
  bundlerEnv,
  ruby,
}: let
  # the magic which will include gemset.nix
  gems = bundlerEnv {
    name = "linguist-env";
    inherit ruby;
    gemdir = ./.;
  };
in
  gems.gems.github-linguist
  // {
    name = "github-linguist";
  }
