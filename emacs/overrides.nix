{ elispTreeSitterVersion
, elispTreeSitterLangsVersion
}:
{ system, pkgs, emacs }:
_eself:
esuper:
{
  vterm = esuper.vterm.overrideAttrs (old: {
    # Based on the configuration in nixpkgs available at the following URL:
    # https://github.com/NixOS/nixpkgs/blob/af21d41260846fb9c9840a75e310e56dfe97d6a3/pkgs/applications/editors/emacs/elisp-packages/melpa-packages.nix#L483
    nativeBuildInputs = [ pkgs.cmake pkgs.gcc ];
    buildInputs = old.buildInputs ++ [ pkgs.libvterm-neovim ];
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

  tsc = esuper.tsc.overrideAttrs (old:
    let
      baseUrl = "https://github.com/emacs-tree-sitter/elisp-tree-sitter/releases/download/${elispTreeSitterVersion}";
      dynName =
        if system == "x86_64-linux"
        then "tsc-dyn.so"
        else throw "Unsupported platform";
      sha256 = {
        "tsc-dyn.so" = "0l0gfsvzcra2qwvg241lx7p1m7aiz5sh5kk65prpmqyfg739ascp";
      };
    in
    assert old.version == elispTreeSitterVersion;
    {
      tscDyn = builtins.fetchurl {
        url = "${baseUrl}/${dynName}";
        sha256 = sha256.${dynName};
      };

      preBuild = ''
        cp $tscDyn ${dynName}
        echo -n "${elispTreeSitterVersion}" > DYN-VERSION
      '';
    });

  tree-sitter-langs = esuper.tree-sitter-langs.overrideAttrs (old:
    let
      baseUrl =
        "https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/${elispTreeSitterLangsVersion}";
      os =
        if system == "x86_64-linux"
        then "linux"
        else throw "Unsupported platform";
      sha256 = {
        # 0.10.13
        linux = "1v1db5nlzix4dax769nq5bsrx5fswxafzjqnsci1fxy9v95i53fx";
      };
    in
    {
      bundle = builtins.fetchurl {
        url = "${baseUrl}/tree-sitter-grammars-${os}-${elispTreeSitterLangsVersion}.tar.gz";
        sha256 = sha256.${os};
      };

      preBuild = ''
        ( mkdir bin && cd bin && tar xzf $bundle )
      '';
    }
  );
}
