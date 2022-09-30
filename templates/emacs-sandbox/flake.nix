# You can save this file to ~/emacs and install the profile by running
# `nix run .#emacs-unsafe-installer`.
#
# By changing the name emacs-unsafe to something, you can have as many profiles
# as your want.
{
  description = "Description for the project";

  inputs = {
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self;} {
      imports = [
      ];
      systems = ["x86_64-linux"];

      flake = {
        lib.emacs-sandbox = {
          name = "emacs-unsafe";
          shareNet = true;
          protectHome = false;
          inheritPath = true;
          userEmacsDirectory = "$HOME/emacs";
          extraFeatures = [
          ];
          extraDirsToTryBind = [
            "/run/user/$(id -u)"
          ];
        };
      };

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        packages = {
          emacs-unsafe-installer = pkgs.writeShellScriptBin "emacs-unsafe-installer" ''
            cd $HOME/config
            echo "Building Emacs..."
            if nix profile install .#emacs-sandboxed \
              --override-input emacs-sandbox $HOME/emacs \
              --priority `date +%s` \
              --print-build-logs; then
                echo Installed
                ${pkgs.notify-desktop}/bin/notify-desktop \
                  -t 5000 "Reinstalled Emacs to the profile"
            else
              ${pkgs.notify-desktop}/bin/notify-desktop \
                -t 5000 "Failed to install Emacs to the profile"
            fi
          '';

          uninstall-old-versions = pkgs.writeShellScriptBin "uninstall-old-versions" ''
            nix profile list \
             | grep -E '-emacs-unsafe$' \
             | cut -d' ' -f1 \
             | head --lines=-1 \
             | xargs nix profile remove
          '';
        };
      };
    };
}
