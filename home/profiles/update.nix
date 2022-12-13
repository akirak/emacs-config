{
  pkgs,
  site,
  ...
}: let
  rebuild = pkgs.writeShellScriptBin "rebuild-home" ''
    cd ${site.nixConfigDir}
    system=$(nix eval builtins.currentSystem --impure)
    nix build .#homeconfigurations.$system."${site.username}@${site.hostName}".activationPackage \
      --override-input site ${site.siteConfigDir}
  '';
in {
  home.packages = [rebuild];
}
