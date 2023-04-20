{
  system,
  pkgs,
  emacs,
}: _eself: esuper: {
  akirak = esuper.akirak.overrideAttrs (old: {
    # The libraries are improperly packaged, so disable byte-compilation for now.
    dontByteCompile = true;
  });

  magit = esuper.magit.overrideAttrs (old: {
    # Since magit 3.3.0, magit requires git executable for byte-compilation.
    buildInputs = old.buildInputs ++ [pkgs.git];
  });

  magit-todos = esuper.magit.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [pkgs.git];
  });

  magit-delta = esuper.magit-delta.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [pkgs.git];
  });

  forge = esuper.forge.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [pkgs.git];
  });

  orgit = esuper.orgit.overrideAttrs (old: {
    # Since magit 3.3.0, magit requires git executable for byte-compilation.
    buildInputs = old.buildInputs ++ [pkgs.git];
  });

  # code-review = esuper.code-review.overrideAttrs (old: {
  #   buildInputs = old.buildInputs ++ [pkgs.git];
  # });

  pdf-tools = esuper.pdf-tools.overrideAttrs (old: {
    CXXFLAGS = "-std=c++17";

    nativeBuildInputs = [
      pkgs.autoconf
      pkgs.automake
      pkgs.pkg-config
      pkgs.removeReferencesTo
    ];
    buildInputs = old.buildInputs ++ [pkgs.libpng pkgs.zlib pkgs.poppler];
    preBuild = ''
      make server/epdfinfo
      cp server/epdfinfo .
      rm -r Makefile lisp server
    '';
  });

  vterm = esuper.vterm.overrideAttrs (old: {
    # Based on the configuration in nixpkgs available at the following URL:
    # https://github.com/NixOS/nixpkgs/blob/af21d41260846fb9c9840a75e310e56dfe97d6a3/pkgs/applications/editors/emacs/elisp-packages/melpa-packages.nix#L483
    nativeBuildInputs = [pkgs.cmake pkgs.gcc];
    buildInputs = old.buildInputs ++ [pkgs.libvterm-neovim];
    cmakeFlags = [
      "-DEMACS_SOURCE=${emacs.src}"
    ];
    preBuild = ''
      mkdir -p build
      cd build
      cmake ..
      make
      install -m444 -t . ../*.so
      install -m600 -t . ../*.el
      cp -r -t . ../etc
      rm -rf {CMake*,build,*.c,*.h,Makefile,*.cmake}
    '';
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
