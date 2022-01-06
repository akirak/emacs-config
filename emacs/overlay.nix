{ twist, org-babel, inventories }:
final: prev:
let
  inherit (prev) tangleOrgBabelFile emacsPgtkGcc emacsTwist;
  org = org-babel.lib;
  inherit (twist.lib { inherit (prev) lib; }) parseSetup;

  initFile = tangleOrgBabelFile "init.el" ./emacs-config.org {
    processLines = org.excludeHeadlines (org.tag "ARCHIVE");
  };

  compatEl = builtins.path {
    name = "compat.el";
    path = ./compat.el;
  };

  extraConfigFile = ./extras.org;

  extraFile = f: tangleOrgBabelFile "extra-init.el" ./extras.org {
    processLines = lines: prev.lib.pipe lines [
      (org.excludeHeadlines (org.tag "ARCHIVE"))
      f
    ];
  };

  elispTreeSitterVersion = "0.16.1";
  elispTreeSitterLangsVersion = "0.10.13";

  makeEmacsConfiguration = initFiles: (emacsTwist {
    inventories = [
      {
        type = "melpa";
        path = ./recipes/overrides;
      }
    ] ++ inventories ++ [
      {
        type = "melpa";
        path = ./recipes/fallbacks;
      }
    ];
    inherit initFiles;
    extraPackages = [
      "setup"
    ];
    initParser = parseSetup { };
    emacsPackage = emacsPgtkGcc.overrideAttrs (_: { version = "29.0.50"; });
    lockDir = ./sources;
    inputOverrides = {
      bufler = _: _: {
        origin = {
          type = "github";
          owner = "akirak";
          repo = "bufler.el";
          ref = "fix-cl-macs";
        };
      };
      tsc = _: _: {
        origin = {
          type = "github";
          owner = "emacs-tree-sitter";
          repo = "elisp-tree-sitter";
          ref = elispTreeSitterVersion;
        };
      };
      tree-sitter = _: _: {
        origin = {
          type = "github";
          owner = "emacs-tree-sitter";
          repo = "elisp-tree-sitter";
          ref = elispTreeSitterVersion;
        };
      };
      tree-sitter-langs = _: _: {
        origin = {
          type = "github";
          owner = "emacs-tree-sitter";
          repo = "tree-sitter-langs";
          ref = elispTreeSitterLangsVersion;
        };
      };
    };
  }).overrideScope' (self: super: {
    elispPackages = super.elispPackages.overrideScope' (eself: esuper: {
      vterm = esuper.vterm.overrideAttrs (old: {
        # Based on the configuration in nixpkgs available at the following URL:
        # https://github.com/NixOS/nixpkgs/blob/af21d41260846fb9c9840a75e310e56dfe97d6a3/pkgs/applications/editors/emacs/elisp-packages/melpa-packages.nix#L483
        nativeBuildInputs = [ final.cmake final.gcc ];
        buildInputs = old.buildInputs ++ [ final.libvterm-neovim ];
        cmakeFlags = [
          "-DEMACS_SOURCE=${self.emacs.src}"
        ];
        preBuild = ''
          cmake
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
            if prev.system == "x86_64-linux"
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
            if prev.system == "x86_64-linux"
            then "linux"
            else throw "Unsupported platform";
          sha256 = {
            # 0.10.13
            linux = "1v1db5nlzix4dax769nq5bsrx5fswxafzjqnsci1fxy9v95i53fx";
            # 0.10.14
            # linux = "1ngbjnm75wqi8yj0s0jsxxfvypx3c342a9s2b3x10nj659d890wb";
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
    });
  });
in
{
  emacsConfigurations = {
    # Used to generate lock files
    full = makeEmacsConfiguration [
      initFile
      compatEl
      (extraFile prev.lib.id)
    ];
    basic = makeEmacsConfiguration [
      initFile
    ];
    compat = makeEmacsConfiguration [
      initFile
      compatEl
    ];
    beancount = makeEmacsConfiguration [
      initFile
      compatEl
      (extraFile (org.selectHeadlines (org.headlineText "beancount")))
    ];
  };
}
