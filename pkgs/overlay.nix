final: prev:
{
  github-linguist = prev.callPackage ./development/github-linguist { };

  shippori-mincho = prev.callPackage ./fonts/shippori-mincho.nix { };
}
