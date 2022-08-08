{
  elispTreeSitterVersion,
  elispTreeSitterLangsVersion,
}: {
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

  queue = esuper.queue.overrideAttrs (old: {
    outputs = ["out"];
  });

  tsc = esuper.tsc.overrideAttrs (old: let
    baseUrl = "https://github.com/emacs-tree-sitter/elisp-tree-sitter/releases/download/${elispTreeSitterVersion}";
    dynName =
      if system == "x86_64-linux"
      then "tsc-dyn.so"
      else throw "Unsupported platform";
    sha256 = {
      "tsc-dyn.so" = "08cpf2rzd364h5x4lp4q819y5zj3dlb47blpg0fdw90dsv6q7cpp";
    };
  in
    assert old.version == elispTreeSitterVersion; {
      tscDyn = builtins.fetchurl {
        url = "${baseUrl}/${dynName}";
        sha256 = sha256.${dynName};
      };

      preBuild = ''
        cp $tscDyn ${dynName}
        echo -n "${elispTreeSitterVersion}" > DYN-VERSION
      '';
    });

  tree-sitter-langs = esuper.tree-sitter-langs.overrideAttrs (
    old: let
      baseUrl = "https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/${elispTreeSitterLangsVersion}";
      os =
        if system == "x86_64-linux"
        then "linux"
        else throw "Unsupported platform";
      sha256 = {
        linux = "0m4w8dlphmzir00syx188f53lijpb5rih7p7by477mlsfj8r9lki";
      };
    in {
      bundle = builtins.fetchurl {
        url = "${baseUrl}/tree-sitter-grammars-${os}-${elispTreeSitterLangsVersion}.tar.gz";
        sha256 = sha256.${os};
      };

      preBuild = ''
        ( mkdir bin && cd bin && tar xzf $bundle )
      '';
    }
  );

  gleam-mode = esuper.gleam-mode.overrideAttrs (old: rec {
    tree-sitter-gleam = builtins.fetchurl {
      url = "https://github.com/J3RN/tree-sitter-gleam/archive/f13d9d86f0c8ea7505dfeaff81a92def444877ae.tar.gz";
      sha256 = "1wljx1cp751pnywzn8kzgr53981a5nh4imcsa2hylbmx0a46kby9";
    };

    preBuild = ''
      install -d tree-sitter-gleam
      tar zxf ${tree-sitter-gleam} --directory=tree-sitter-gleam --strip-components=1

      emacs --batch -L . -l gleam-mode -f gleam-mode--compile-grammar
    '';
  });
}
