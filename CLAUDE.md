## Developing

### Emacs Lisp

This repository provides a configuration for GNU Emacs.

The main entry point is emacs-config.org, which is a literate programming file
in Org mode. Configurations are organized under per-package heading, grouped by
configuration sections.

Functions that should be byte or native compiled are saved to lisp/akirak-*.el.
If necessary, autoload cookies should be added to public functions.

### Nix

The entire configuration is built with Nix. The `flake.nix` in the repository
root provides entry points for building the configuration for different needs.
The `lock` directory tracks revision of third-party Emacs Lisp packages used in
the configuration.
