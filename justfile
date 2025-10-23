lock:
    nix run .\#lock --impure -L

update-inputs:
    nix flake update melpa gnu-elpa nongnu-elpa gnu-elpa-archive nongnu-elpa-archive

update: update-inputs
    nix run .\#update --impure -L
