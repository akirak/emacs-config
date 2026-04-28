{
  paths,
  overlay,
}:
{ inputs, ... }:
let
  inherit (inputs) denix;

  extraOverlay =
    _final: prev:
    let
      inherit (prev.stdenv.hostPlatform) system;
    in
    {
      ai-tools = inputs.llm-agents.packages.${system};
      # nix-index = inputs.nix-index-database.packages.${system}.nix-index-with-db;
      zen-browser = inputs.zen-browser.packages.${system}.default;
      tix = inputs.tix.packages.${system}.default;
    };

  overlays = [
    overlay
    extraOverlay
  ];

  mkConfigurations =
    moduleSystem:
    denix.lib.configurations {
      inherit moduleSystem paths;

      homeManagerUser = "akirakomamura";

      extensions = with denix.lib.extensions; [
        args
        (base.withConfig (
          _final: _prev: {
            args.enable = true;

            # Based on https://github.com/yunfachi/nix-config/blob/9ba35d6fc96a4eb86db72c91a0fc74e636c71f82/flake.nix#L25
            hosts.features = {
              enable = true;
              generateIsFeatured = true;
              features = [
                "cli"
                "gui"
                "coding"
              ];
              defaultByHostType = {
                desktop = [
                  "cli"
                  "gui"
                  "coding"
                ];
                server = [ ];
              };
            };
          }
        ))
      ];
      specialArgs = {
        inherit
          moduleSystem
          inputs
          overlays
          ;
      };
    };
in
{
  flake = {
    homeConfigurations = mkConfigurations "home";

    overlays.extras = extraOverlay;
  };
}
