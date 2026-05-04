lock:
    nix run .\#lock --impure -L

update: update-registries
    nix run .\#update --impure -L

update-registries:
    nix flake update --flake path:./nix/flake-parts/partitions/packages
