{
  inputs = {
    flake-pins.url = "github:akirak/flake-pins";

    denix = {
      url = "github:yunfachi/denix";
      inputs.nixpkgs.follows = "flake-pins/unstable";
      inputs.home-manager.follows = "flake-pins/home-manager-unstable";
      # inputs.nix-darwin.follows = "flake-pins/nix-darwin";
    };

    # Extra packages used in the home-manager configuration
    xremap.url = "github:xremap/nix-flake";
    llm-agents.url = "github:numtide/llm-agents.nix";

    tix = {
      url = "github:JRMurr/tix";
      # The upstream follows the nixos-unstable channel.
      # It must be nixpkgs-unstable (flake-pins/nixpkgs in this flake).
      inputs.nixpkgs.follows = "flake-pins/unstable";
    };

    # Don't override the nixpkgs input so I can utilize the binary cache.
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
  };

  outputs = _: { };
}
