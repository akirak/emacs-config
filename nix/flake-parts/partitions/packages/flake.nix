# Copyright (C) 2026 Akira Komamura
# SPDX-License-Identifier: MIT

{
  inputs = {
    nix-filter.url = "github:numtide/nix-filter";

    # Emacs Twist
    twist-overrides.url = "github:emacs-twist/overrides";

    # Package registries for Twist
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };
    gnu-elpa = {
      # Use a GitHub mirror for a higher availability
      url = "github:elpa-mirrors/elpa";
      # url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    nongnu-elpa = {
      # Use a GitHub mirror for a higher availability
      url = "github:elpa-mirrors/nongnu";
      # url = "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
      flake = false;
    };
  };

  outputs = _: {};
}
