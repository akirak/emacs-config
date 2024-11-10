import ./emacs/profiles.nix {
  inherit lib;
  defaultFeatures = import inputs.emacs-config-features;
} emacs-config
