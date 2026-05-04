# Repository Guidelines

## Project Structure & Module Organization

- `emacs-config.org`: main literate Emacs configuration; `README.org` links to it.
- `early-init.el`: early Emacs startup settings.
- `lisp/`: custom Emacs Lisp modules using the `akirak-*.el` naming pattern.
- `recipes/`: package recipes consumed by the Emacs/Twist build.
- `lock/`: generated and pinned package state.
- `nix/twist/`: Nix code for building the Emacs environment.
- `nix/lib/`: reusable Nix and helper utilities.
- `nix/flake-parts/`: flake partition definitions for shells, packages, and configs.
- `nix/denix/`: Denix/Home Manager modules.
- `nix/lsp-proxy-config.nix`: configuration for language servers.
- `.github/workflows/`: CI and update automation.
- `justfile`: Shorthands for human operations.
## Build, Test, and Development Commands
### Initial setup
- `nix develop`: enter the development shell with the configured pre-commit hooks and formatting tools.
### Build and check
- `nix fmt`: Reformat the Nix code.
- `nix build .#checks.x86_64-linux.treefmt -L`: Check the format.
- `nix build .#emacs-config.depsCheck --print-build-logs`: verify Emacs package dependency resolution, matching CI.
- `nix build .#emacs-config.depsCheck -L`: verify Emacs package dependency resolution, matching CI.
>>>>>>> b621b2be (squash! agents: Init)
- `nix build .#checks.x86_64-linux.elisp-packages -L`: Build the Emacs Lisp packages.
- `nix build .#checks.x86_64-linux.homeConfiguration -L`: Build the sample home-manager configuration.
### Updating
- `nix run .#lock --impure -L`: Fill missing lock entries for the Emacs Lisp packages.
- `nix flake update --flake path:./nix/flake-parts/partitions/packages`: Update the Emacs package registries.
- `nix run .#update --impure -L`: update ELPA inputs, then run the repository update app.
- `nix run .#update-ai-models`: Update `ai-model-list.txt`.
- `nix run .#update-elisp-lock`: Commit package updates by author.
## Coding Style & Naming Conventions
Use two-space indentation for Nix files and keep module files focused by domain. Format Nix with `nixfmt`; unused Nix expressions are checked by `deadnix`. These are enforced by running `nix fmt`.

Emacs Lisp files should use the `akirak-<feature>.el` pattern and provide matching feature names.

Keep generated lock files and recipe updates separate from hand-written configuration changes when possible.

All files should have a `SPDX-License-Identifier` header. Emacs Lisp files in `lisp/` directory must have `GPL-3.0-or-later`. Most other files in the repository should be `MIT`, but there can be few exceptions. Keep updating the copyright year in LICENSES/MIT.txt.
## Testing Guidelines
There is no standalone test directory. Treat Nix builds and flake checks as the test suite.
## Commit & Pull Request Guidelines
The commit message should be prefixed with the following keywords:

- `lisp/*: ` if `lisp/akirak-*.el` is changed.
- `emacs:` if the Emacs configuration `emacs-config.org` is changed.

No specific rule when other files are changed.

Keep commits focused and avoid mixing generated lock updates with unrelated edits.
