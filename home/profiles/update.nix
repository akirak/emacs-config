{
  pkgs,
  site,
  ...
}: let
  rebuild = pkgs.writeShellScriptBin "rebuild-home" ''
    cd ${site.nixConfigDir}
    system=$(nix eval --expr builtins.currentSystem --impure --raw)
    nix build .#homeConfigurations.$system."${site.username}@${site.hostName}".activationPackage \
      --override-input site ${site.siteConfigDir}
  '';
in {
  home.packages = [rebuild];
}
