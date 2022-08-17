final: prev: {
  github-linguist = prev.callPackage ./development/github-linguist {};

  shippori-mincho = prev.callPackage ./fonts/shippori-mincho.nix {};
  jetbrains-mono-nerdfont = prev.callPackage ./fonts/jetbrains-mono-nerdfont.nix {};

  bubblewrapGUI = prev.callPackage ./security/bubblewrap-gui.nix {};

  emacsSandboxed = prev.callPackage ./development/emacs-sandboxed {} {
    inherit (final) bubblewrapGUI;
  };

  readability-cli = prev.callPackage ./media/readability-cli {
    pkgs = prev;
  };

  nodePackages =
    prev.nodePackages
    // {
      mermaid-cli = prev.nodePackages.mermaid-cli.overrideAttrs (old: {
        passthru =
          (old.passthru or {})
          // {
            exePath = "/bin/mmdc";
          };
      });
    };
}
