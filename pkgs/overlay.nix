final: prev:
{
  github-linguist = prev.callPackage ./development/github-linguist { };

  shippori-mincho = prev.callPackage ./fonts/shippori-mincho.nix { };
  jetbrains-mono-nerdfont = prev.callPackage ./fonts/jetbrains-mono-nerdfont.nix { };
}
