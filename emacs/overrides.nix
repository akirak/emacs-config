{
  system,
  pkgs,
  emacs,
}: _eself: esuper:
builtins.intersectAttrs
esuper
{
  akirak = esuper.akirak.overrideAttrs (old: {
    # The libraries are improperly packaged, so disable byte-compilation for now.
    dontByteCompile = true;
  });

  emacsql-sqlite = esuper.emacsql-sqlite.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [pkgs.sqlite];

    postBuild = ''
      cd sqlite
      make
      cd ..
    '';
  });
}
