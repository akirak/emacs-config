{
  delib,
  lib,
  ...
}:
delib.module {
  name = "agent-skills";

  options =
    with delib;
    moduleOptions (args: {
      enable = boolOption false;

      roots = listOfOption str (lib.optional args.myconfig.codex.enable ".codex/skills");

      sources = attrsOfOption (submodule {
        options = {
          path = noNullDefault (pathOption null);
          priority = intOption 1;
        };
      }) { };
    });

  home.ifEnabled =
    { cfg, ... }:
    let
      skillDirs = lib.pipe (builtins.attrValues cfg.sources) [
        (builtins.sort (a: b: a.priority > b.priority))
        (lib.concatMap (
          { path, ... }:
          lib.pipe (builtins.readDir path) [
            (lib.filterAttrs (_name: type: type == "directory"))
            (lib.mapAttrsToList (
              name: _type: {
                inherit name;
                source = path + "/${name}";
              }
            ))
          ]
        ))
      ];
    in
    {
      # Because of the presense of system skills (e.g. ~/.codex/skills/.system),
      # you can't set the roots directory.
      home.file = lib.listToAttrs (
        lib.zipListsWith (
          root: { name, source }: lib.nameValuePair ("${root}/${name}") { inherit source; }
        ) cfg.roots skillDirs
      );
    };
}
