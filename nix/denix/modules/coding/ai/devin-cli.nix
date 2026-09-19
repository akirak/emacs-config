{
  delib,
  lib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "devin-cli";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.codingFeatured;
    };

  home.ifEnabled =
    let
      inherit (pkgs.stdenv.hostPlatform) system;

      releases = {
        # aarch64-darwin = {
        # target = "aarch64-apple-darwin";
        # hash = "sha256-wLl/gZe/POiV/xSqGSV8URFUtJoKGVu6SWKsteR1xo4=";
        # };
        # aarch64-linux = {
        # target = "aarch64-unknown-linux";
        # hash = "sha256-pjEk7S+EBqXUShYvousFucDyGKaxMeLKEzXUozXHCmw=";
        # };
        # x86_64-darwin = {
        # target = "x86_64-apple-darwin";
        # hash = "sha256-RyXWsNu/b3HYM7VIlGnci1xKT5KZJvlNUAlSpMt7utg=";
        # };
        x86_64-linux = {
          target = "x86_64-unknown-linux";
          hash = "sha256-fKxvVzm6Oj5VQvO3+gftkC1t+5bKIuTGOuhMA7t9tHw=";
        };
      };

      release = releases.${system};

      version = "3000.10.21";

      devin-cli = pkgs.callPackage (
        { stdenvNoCC }:
        stdenvNoCC.mkDerivation {
          pname = "devin";
          inherit version;

          src = pkgs.fetchurl {
            url = "https://static.devin.ai/cli/${version}/devin-${version}-${release.target}.tar.gz";
            inherit (release) hash;
          };

          sourceRoot = ".";

          installPhase = ''
            runHook preInstall
            mkdir -p "$out"
            cp -R bin share "$out/"
            runHook postInstall
          '';

          # Preserve the vendor binary, including its Darwin code signature.
          dontStrip = true;

          meta = {
            description = "Command-line interface for Devin";
            homepage = "https://devin.ai";
            license = lib.licenses.unfree;
            mainProgram = "devin";
            platforms = builtins.attrNames releases;
            sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
          };
        }
      ) { };
    in
    {
      home.packages = [
        devin-cli
      ];
    };
}
