{lib}:
with builtins; let
  moduleDir = ./profiles;

  profiles = lib.pipe (readDir moduleDir) [
    attrNames
    (filter (name: name != "default.nix"))
    (map (name: {
      name = lib.removeSuffix ".nix" name;
      value = moduleDir + "/${name}";
    }))
    listToAttrs
  ];

  suites = lib.mapAttrs (_: imports: {inherit imports;}) rec {
    base = [
      ./profiles/core.nix
      ./profiles/zsh.nix
      ./profiles/gpg.nix
    ];
    desktop =
      base
      ++ [
        ./profiles/development.nix
        ./profiles/writing.nix
        ./profiles/graphical.nix
        ./profiles/git-annex.nix
        ./profiles/recoll.nix
      ];
    xmonad =
      desktop
      ++ [
        ./profiles/wm/xmonad
      ];
    river =
      desktop
      ++ [
        ./profiles/wm/river
      ];
    work = [
      ./profiles/office.nix
    ];
    personal = [
      ./profiles/syncthing.nix
      ./profiles/dropbox.nix
    ];
  };
in
  profiles // suites
