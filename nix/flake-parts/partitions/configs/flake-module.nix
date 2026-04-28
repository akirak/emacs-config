{
  paths,
  makeEmacsEnvWithPkgs,
  emacsConfigOrg,
}:
{ inputs, ... }:
let
  inherit (inputs) denix;

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
        inherit moduleSystem inputs makeEmacsEnvWithPkgs emacsConfigOrg;
      };
    };
in
{
  flake = {
    homeConfigurations = mkConfigurations "home";
  };
}
