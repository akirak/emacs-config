final: prev: {
  bubblewrapGUI = prev.callPackage ./security/bubblewrap-gui.nix {};

  emacsSandboxed = prev.callPackage ./development/emacs-sandboxed {} {
    inherit (final) bubblewrapGUI;
  };
}
