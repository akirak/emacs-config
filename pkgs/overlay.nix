final: prev:
{
  github-linguist = prev.callPackage ./development/github-linguist { };

  shippori-mincho = prev.callPackage ./fonts/shippori-mincho.nix { };
  jetbrains-mono-nerdfont = prev.callPackage ./fonts/jetbrains-mono-nerdfont.nix { };

  bubblewrapGUI = prev.callPackage ./security/bubblewrap-gui.nix { };

  emacsSandboxed = prev.callPackage ./development/emacs-sandboxed { } {
    inherit (final) bubblewrapGUI;
  };
}
