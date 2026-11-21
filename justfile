lock:
    nix run .\#lock --impure -L

update: update-registries
    nix run .\#update --impure -L

update-registries:
    nix flake update --flake path:./nix/flake-parts/partitions/packages

update-flakes:
    nix flake update flake-pins
    nix flake update --inputs-from github:akirak/flake-pins
    for dir in nix/flake-parts/partitions/*; do nix flake update --flake "path:./$dir" $(grep -q '"flake-pins"' "$dir/flake.lock" && echo flake-pins) --inputs-from github:akirak/flake-pins; done
