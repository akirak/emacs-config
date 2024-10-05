{ inputs, ... }:
{
  imports = [ inputs.git-hooks-nix.flakeModule ];

  perSystem =
    {
      pkgs,
      system,
      config,
      ...
    }:
    {
      pre-commit = {
        check.enable = true;
        settings.hooks = import ./hooks.nix {
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              (_: prev: {
                # Add packages for use in the hooks.
                emacs-with-pkgs = prev.emacs.pkgs.withPackages (epkgs: [
                  epkgs.org-ql
                  epkgs.org-make-toc
                ]);
                flake-no-path = inputs.flake-no-path.defaultPackage.${system};
              })
            ];
          };
        };
      };

      devShells = {
        default = config.pre-commit.devShell;
      };
    };
}
