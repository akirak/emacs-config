# Flake for Firefox

With this flake, you can install the beta version of Firefox using the following command to your profile:

```
nix profile install "github:akirak/nix-config?dir=extras/firefox#firefox-beta-bin" --impure --no-write-lock-file
```

This allows installing Firefox independently from the base NixOS, so the browser can be updated more frequently without messing the repository history.

The following command uninstalls Firefox:

```sh
nix profile remove firefox-beta-bin.x86_64-linux
```
