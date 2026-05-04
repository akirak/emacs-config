# A configuration for lsp-proxy in Nix.
#
# This configuration takes an inspiration from https://apribase.net/2026/03/09/lsp-proxy/.
# See https://github.com/jadestrong/lsp-proxy#language-configuration for the
# configuration schema.
{
  lib,
  formats,
  copilot-language-server,
}:
let
  defaultLanguageServers = {
    copilot = {
      command = lib.getExe copilot-language-server;
      args = [ "--stdio" ];
    };
  };

  fromEglotStyleSettings = eglotStyleSettings: {
    language-server =
      defaultLanguageServers // (lib.foldl' (acc: a: acc // a.servers) { } eglotStyleSettings);

    language = lib.flatten (
      builtins.map (
        a:
        (builtins.map (
          language:
          language
          // {
            language-servers = (a.serverNames or (builtins.attrNames a.servers)) ++ [ "copilot" ];
          }
        ) a.languages)
      ) eglotStyleSettings
    );
  };
in
(formats.toml { }).generate "languages.toml" (fromEglotStyleSettings [
  # TypeScript, JavaScript, and React
  {
    languages = [
      {
        name = "typescript";
        language-id = "typescript";
        file-types = [ "ts" ];
      }
      {
        name = "typescript-react";
        language-id = "typescriptreact";
        file-types = [ "tsx" ];
      }
      {
        name = "javascript";
        language-id = "javascript";
        file-types = [
          "js"
          "mjs"
          "cjs"
        ];
      }
      {
        name = "javascript-react";
        language-id = "javascriptreact";
        file-types = [ "jsx" ];
      }
    ];
    serverNames = [
      "rass-ts"
      "oxlint"
      "tsgo"
      "typescript-language-server"
      "deno"
    ];
    servers = {
      rass-ts = {
        command = "rass";
        args = [ "ts" ];
      };
      oxlint = {
        command = "oxlint";
        args = [
          "--lsp"
        ];
      };
      tsgo = {
        command = "tsgo";
        args = [
          "--lsp"
          "--stdio"
        ];
      };
      typescript-language-server = {
        command = "typescript-language-server";
        args = [
          "--stdio"
        ];
      };
      deno = {
        command = "deno";
        args = [
          "lsp"
        ];
      };
    };
  }

  # Nix
  {
    languages = [
      {
        name = "nix";
        file-types = [ "nix" ];
      }
    ];
    serverNames = [
      "tix"
      "typenix"
      "nil"
      "rnix-lsp"
      "nixd"
    ];
    servers = {
      tix = {
        command = "tix";
        args = [ "lsp" ];
      };
      typenix = {
        command = "typenix";
        args = [
          "--lsp"
          "--stdio"
        ];
      };
      nil = {
        command = "nil";
      };
      rnix-lsp = {
        command = "rnix-lsp";
      };
      nixd = {
        command = "nixd";
      };
    };
  }

  # Justfile
  {
    languages = [
      {
        name = "just";
        file-types = [
          { glob = "justfile"; }
          { glob = "Justfile"; }
          { glob = ".justfile"; }
        ];
      }
    ];
    servers = {
      just-lsp = {
        command = "just-lsp";
      };
    };
  }

  # Lean 4
  {
    languages = [
      {
        name = "lean";
        language-id = "lean";
        file-types = [ "lean" ];
      }
    ];
    servers = {
      lake = {
        command = "lake";
        args = [ "serve" ];
      };
    };
  }

  # SQL
  {
    languages = [
      {
        name = "sql";
        file-types = [ "sql" ];
      }
    ];
    servers = {
      sqlmesh_lsp = {
        command = "sqlmesh_lsp";
      };
    };
  }

  # Svelte
  {
    languages = [
      {
        name = "svelte";
        file-types = [ "svelte" ];
      }
    ];
    servers = {
      svelteserver = {
        command = "svelteserver";
        args = [ "--stdio" ];
      };
    };
  }

  # HCL
  {
    languages = [
      {
        name = "hcl";
        file-types = [
          "hcl"
          "tf"
          "tfvars"
        ];
      }
    ];
    serverNames = [
      "terraform-ls"
      "terraform-lsp"
    ];
    servers = {
      terraform-ls = {
        command = "terraform-ls";
        args = [ "serve" ];
      };
      terraform-lsp = {
        command = "terraform-lsp";
        args = [ "--stdio" ];
      };
    };
  }

  # OCaml
  {
    languages = [
      {
        name = "ocaml";
        language-id = "ocaml";
        file-types = [ "ml" ];
      }
      {
        name = "ocaml-interface";
        language-id = "ocamli";
        file-types = [ "mli" ];
      }
      {
        name = "dune";
        language-id = "dune";
        file-types = [
          { glob = "dune"; }
          { glob = "dune-project"; }
        ];
      }
    ];
    servers = {
      ocamllsp = {
        command = "ocamllsp";
      };
    };
  }

  # Coq (Rocq)
  {
    languages = [
      {
        name = "coq";
        file-types = [ "v" ];
      }
    ];
    servers = {
      coq-lsp = {
        command = "coq-lsp";
      };
    };
  }

  # Gleam
  {
    languages = [
      {
        name = "gleam";
        file-types = [ "gleam" ];
      }
    ];
    servers = {
      gleam = {
        command = "gleam";
        args = [ "lsp" ];
      };
    };
  }

  # Zig
  {
    languages = [
      {
        name = "zig";
        file-types = [ "zig" ];
      }
    ];
    servers = {
      zls = {
        command = "zls";
      };
    };
  }

  # F#
  {
    languages = [
      {
        name = "fsharp";
        file-types = [
          "fs"
          "fsi"
          "fsx"
          "fsscript"
        ];
      }
    ];
    servers = {
      fsautocomplete = {
        command = "fsautocomplete";
        args = [ "--adaptive-lsp-server-enabled" ];
      };
    };
  }

  # Elixir
  {
    languages = [
      {
        name = "elixir";
        file-types = [
          "ex"
          "exs"
        ];
      }
      {
        name = "heex";
        file-types = [ "heex" ];
      }
    ];
    # The Emacs config also passes server-specific initialization options for
    # nextls. Keep the server entry here conservative until those options are
    # expressed explicitly in lsp-proxy's schema.
    servers = {
      lexical = {
        command = "lexical";
      };
      nextls = {
        command = "nextls";
        args = [ "--stdio=true" ];
      };
      elixir-ls = {
        command = "elixir-ls";
      };
    };
  }

  # Astro
  {
    languages = [
      {
        name = "astro";
        file-types = [ "astro" ];
      }
    ];
    # The Emacs config includes initialization options for the TypeScript SDK.
    # This file keeps the command mapping aligned without assuming the exact
    # lsp-proxy encoding for those options.
    servers = {
      astro-ls = {
        command = "astro-ls";
        args = [ "--stdio" ];
      };
    };
  }

  # Java
  {
    languages = [
      {
        name = "java";
        file-types = [ "java" ];
      }
    ];
    serverNames = [
      "jdtls"
      "java-language-server"
    ];
    # The Emacs config conditionally adds a debug bundle to jdtls when the jar
    # exists in the user's Emacs directory. That dynamic path is intentionally
    # left out here.
    servers = {
      jdtls = {
        command = "jdtls";
      };
      java-language-server = {
        command = "java-language-server";
      };
    };
  }

  # Rust
  {
    languages = [
      {
        name = "rust";
        file-types = [ "rs" ];
      }
    ];
    servers = {
      rust-analyzer = {
        command = "rust-analyzer";
      };
    };
  }

  # CMake
  {
    languages = [
      {
        name = "cmake";
        file-types = [
          "cmake"
          { glob = "CMakeLists.txt"; }
        ];
      }
    ];
    serverNames = [
      "neocmakelsp"
      "cmake-language-server"
    ];
    servers = {
      neocmakelsp = {
        command = "neocmakelsp";
        args = [ "stdio" ];
      };
      cmake-language-server = {
        command = "cmake-language-server";
      };
    };
  }

  # Vim
  {
    languages = [
      {
        name = "vim";
        file-types = [
          "vim"
          { glob = ".vimrc"; }
        ];
      }
    ];
    servers = {
      vim-language-server = {
        command = "vim-language-server";
        args = [ "--stdio" ];
      };
    };
  }

  # Python
  {
    languages = [
      {
        name = "python";
        file-types = [ "py" ];
      }
    ];
    serverNames = [
      "rass-python"
      "pylsp"
      "pyls"
      "basedpyright-langserver"
      "pyright-langserver"
      "pyrefly"
      "ty"
      "jedi-language-server"
      "ruff"
      "ruff-lsp"
    ];
    servers = {
      rass-python = {
        command = "rass";
        args = [ "python" ];
      };
      pylsp = {
        command = "pylsp";
      };
      pyls = {
        command = "pyls";
      };
      basedpyright-langserver = {
        command = "basedpyright-langserver";
        args = [ "--stdio" ];
      };
      pyright-langserver = {
        command = "pyright-langserver";
        args = [ "--stdio" ];
      };
      pyrefly = {
        command = "pyrefly";
        args = [ "lsp" ];
      };
      ty = {
        command = "ty";
        args = [ "server" ];
      };
      jedi-language-server = {
        command = "jedi-language-server";
      };
      ruff = {
        command = "ruff";
        args = [ "server" ];
      };
      ruff-lsp = {
        command = "ruff-lsp";
      };
    };
  }

  # JSON
  {
    languages = [
      {
        name = "json";
        file-types = [
          "json"
          "jsonc"
        ];
      }
    ];
    serverNames = [
      "vscode-json-language-server"
      "vscode-json-languageserver"
      "json-languageserver"
    ];
    servers = {
      vscode-json-language-server = {
        command = "vscode-json-language-server";
        args = [ "--stdio" ];
      };
      vscode-json-languageserver = {
        command = "vscode-json-languageserver";
        args = [ "--stdio" ];
      };
      json-languageserver = {
        command = "json-languageserver";
        args = [ "--stdio" ];
      };
    };
  }

  # Shell
  {
    languages = [
      {
        name = "bash";
        file-types = [
          "sh"
          "bash"
        ];
      }
    ];
    servers = {
      bash-language-server = {
        command = "bash-language-server";
        args = [ "start" ];
      };
    };
  }

  # PHP
  {
    languages = [
      {
        name = "php";
        file-types = [ "php" ];
      }
    ];
    serverNames = [
      "phpactor"
      "php-language-server"
    ];
    servers = {
      phpactor = {
        command = "phpactor";
        args = [ "language-server" ];
      };
      php-language-server = {
        command = "php";
        args = [ "vendor/felixfbecker/language-server/bin/php-language-server.php" ];
      };
    };
  }

  # C, C++, and Objective-C
  {
    languages = [
      {
        name = "c";
        file-types = [
          "c"
          "h"
        ];
      }
      {
        name = "cpp";
        file-types = [
          "cc"
          "cpp"
          "cxx"
          "hh"
          "hpp"
          "hxx"
        ];
      }
      {
        name = "objective-c";
        file-types = [
          "m"
          "mm"
        ];
      }
    ];
    serverNames = [
      "clangd"
      "ccls"
    ];
    servers = {
      clangd = {
        command = "clangd";
      };
      ccls = {
        command = "ccls";
      };
    };
  }

  # Ruby
  {
    languages = [
      {
        name = "ruby";
        file-types = [ "rb" ];
      }
    ];
    servers = {
      ruby-lsp = {
        command = "ruby-lsp";
      };
    };
  }

  # Haskell
  {
    languages = [
      {
        name = "haskell";
        file-types = [
          "hs"
          "lhs"
        ];
      }
    ];
    serverNames = [
      "haskell-language-server-wrapper"
      "static-ls"
    ];
    servers = {
      haskell-language-server-wrapper = {
        command = "haskell-language-server-wrapper";
        args = [ "--lsp" ];
      };
      static-ls = {
        command = "static-ls";
      };
    };
  }

  # Elm
  {
    languages = [
      {
        name = "elm";
        file-types = [ "elm" ];
      }
    ];
    servers = {
      elm-language-server = {
        command = "elm-language-server";
      };
    };
  }

  # Mint
  {
    languages = [
      {
        name = "mint";
        file-types = [ "mint" ];
      }
    ];
    servers = {
      mint = {
        command = "mint";
        args = [ "ls" ];
      };
    };
  }

  # Kotlin
  {
    languages = [
      {
        name = "kotlin";
        file-types = [
          "kt"
          "kts"
        ];
      }
    ];
    servers = {
      kotlin-language-server = {
        command = "kotlin-language-server";
      };
    };
  }

  # Go
  {
    languages = [
      {
        name = "go";
        file-types = [ "go" ];
      }
      {
        name = "go-mod";
        file-types = [ { glob = "go.mod"; } ];
      }
      {
        name = "go-work";
        file-types = [ { glob = "go.work"; } ];
      }
    ];
    servers = {
      gopls = {
        command = "gopls";
      };
    };
  }

  # R
  {
    languages = [
      {
        name = "r";
        file-types = [
          "r"
          "R"
        ];
      }
    ];
    servers = {
      r-languageserver = {
        command = "R";
        args = [
          "--slave"
          "-e"
          "languageserver::run()"
        ];
      };
    };
  }

  # Dart
  {
    languages = [
      {
        name = "dart";
        file-types = [ "dart" ];
      }
    ];
    servers = {
      dart = {
        command = "dart";
        args = [
          "language-server"
          "--client-id"
          "emacs.eglot-dart"
        ];
      };
    };
  }

  # Ada
  {
    languages = [
      {
        name = "ada";
        file-types = [
          "adb"
          "ads"
          "ada"
        ];
      }
    ];
    servers = {
      ada-language-server = {
        command = "ada_language_server";
      };
    };
  }

  # GPR
  {
    languages = [
      {
        name = "gpr";
        file-types = [ "gpr" ];
      }
    ];
    servers = {
      ada-language-server-gpr = {
        command = "ada_language_server";
        args = [ "--language-gpr" ];
      };
    };
  }

  # Scala
  {
    languages = [
      {
        name = "scala";
        file-types = [ "scala" ];
      }
    ];
    serverNames = [
      "metals"
      "metals-emacs"
    ];
    servers = {
      metals = {
        command = "metals";
      };
      metals-emacs = {
        command = "metals-emacs";
      };
    };
  }

  # Racket
  {
    languages = [
      {
        name = "racket";
        file-types = [ "rkt" ];
      }
    ];
    servers = {
      racket = {
        command = "racket";
        args = [
          "-l"
          "racket-langserver"
        ];
      };
    };
  }

  # TeX and BibTeX
  {
    languages = [
      {
        name = "tex";
        file-types = [
          "tex"
          "ltx"
          "ctx"
          "texi"
          "texinfo"
        ];
      }
      {
        name = "bibtex";
        file-types = [ "bib" ];
      }
    ];
    serverNames = [
      "digestif"
      "texlab"
    ];
    servers = {
      digestif = {
        command = "digestif";
      };
      texlab = {
        command = "texlab";
      };
    };
  }

  # Erlang
  {
    languages = [
      {
        name = "erlang";
        file-types = [
          "erl"
          "hrl"
        ];
      }
    ];
    servers = {
      elp = {
        command = "elp";
        args = [ "server" ];
      };
    };
  }

  # WebAssembly Text
  {
    languages = [
      {
        name = "wat";
        file-types = [ "wat" ];
      }
    ];
    servers = {
      wat_server = {
        command = "wat_server";
      };
    };
  }

  # YAML
  {
    languages = [
      {
        name = "yaml";
        file-types = [
          "yaml"
          "yml"
        ];
      }
    ];
    servers = {
      yaml-language-server = {
        command = "yaml-language-server";
        args = [ "--stdio" ];
      };
    };
  }

  # TOML
  {
    languages = [
      {
        name = "toml";
        file-types = [ "toml" ];
      }
    ];
    servers = {
      tombi = {
        command = "tombi";
        args = [ "lsp" ];
      };
    };
  }

  # Nickel
  {
    languages = [
      {
        name = "nickel";
        file-types = [ "ncl" ];
      }
    ];
    servers = {
      nls = {
        command = "nls";
      };
    };
  }

  # Nushell
  {
    languages = [
      {
        name = "nushell";
        file-types = [ "nu" ];
      }
    ];
    servers = {
      nu = {
        command = "nu";
        args = [ "--lsp" ];
      };
    };
  }

  # Fennel
  {
    languages = [
      {
        name = "fennel";
        file-types = [ "fnl" ];
      }
    ];
    servers = {
      fennel-ls = {
        command = "fennel-ls";
      };
    };
  }

  # Move
  {
    languages = [
      {
        name = "move";
        file-types = [ "move" ];
      }
    ];
    servers = {
      move-analyzer = {
        command = "move-analyzer";
      };
    };
  }

  # Fortran
  {
    languages = [
      {
        name = "fortran";
        file-types = [
          "f"
          "f90"
          "f95"
          "f03"
          "f08"
          "for"
          "fpp"
        ];
      }
    ];
    servers = {
      fortls = {
        command = "fortls";
      };
    };
  }

  # Futhark
  {
    languages = [
      {
        name = "futhark";
        file-types = [ "fut" ];
      }
    ];
    servers = {
      futhark = {
        command = "futhark";
        args = [ "lsp" ];
      };
    };
  }

  # Lua
  {
    languages = [
      {
        name = "lua";
        file-types = [ "lua" ];
      }
    ];
    serverNames = [
      "lua-language-server"
      "lua-lsp"
    ];
    servers = {
      lua-language-server = {
        command = "lua-language-server";
      };
      lua-lsp = {
        command = "lua-lsp";
      };
    };
  }

  # Yang
  {
    languages = [
      {
        name = "yang";
        file-types = [ "yang" ];
      }
    ];
    servers = {
      yang-language-server = {
        command = "yang-language-server";
      };
    };
  }

  # CSS
  {
    languages = [
      {
        name = "css";
        file-types = [ "css" ];
      }
    ];
    serverNames = [
      "vscode-css-language-server"
      "css-languageserver"
    ];
    servers = {
      vscode-css-language-server = {
        command = "vscode-css-language-server";
        args = [ "--stdio" ];
      };
      css-languageserver = {
        command = "css-languageserver";
        args = [ "--stdio" ];
      };
    };
  }

  # HTML
  {
    languages = [
      {
        name = "html";
        file-types = [
          "html"
          "htm"
        ];
      }
    ];
    serverNames = [
      "vscode-html-language-server"
      "html-languageserver"
    ];
    servers = {
      vscode-html-language-server = {
        command = "vscode-html-language-server";
        args = [ "--stdio" ];
      };
      html-languageserver = {
        command = "html-languageserver";
        args = [ "--stdio" ];
      };
    };
  }

  # Dockerfile
  {
    languages = [
      {
        name = "dockerfile";
        file-types = [
          { glob = "Dockerfile"; }
          { glob = "Dockerfile.*"; }
        ];
      }
    ];
    servers = {
      docker-langserver = {
        command = "docker-langserver";
        args = [ "--stdio" ];
      };
    };
  }

  # Clojure
  {
    languages = [
      {
        name = "clojure";
        file-types = [
          "clj"
          "cljs"
          "cljc"
        ];
      }
    ];
    servers = {
      clojure-lsp = {
        command = "clojure-lsp";
      };
    };
  }

  # C#
  {
    languages = [
      {
        name = "csharp";
        file-types = [ "cs" ];
      }
    ];
    serverNames = [
      "omnisharp"
      "OmniSharp"
      "csharp-ls"
    ];
    servers = {
      omnisharp = {
        command = "omnisharp";
        args = [ "-lsp" ];
      };
      OmniSharp = {
        command = "OmniSharp";
        args = [ "-lsp" ];
      };
      csharp-ls = {
        command = "csharp-ls";
      };
    };
  }

  # Purescript
  {
    languages = [
      {
        name = "purescript";
        file-types = [ "purs" ];
      }
    ];
    servers = {
      purescript-language-server = {
        command = "purescript-language-server";
        args = [ "--stdio" ];
      };
    };
  }

  # Perl
  {
    languages = [
      {
        name = "perl";
        file-types = [
          "pl"
          "pm"
          "t"
        ];
      }
    ];
    servers = {
      perl-language-server = {
        command = "perl";
        args = [
          "-MPerl::LanguageServer"
          "-e"
          "Perl::LanguageServer::run"
        ];
      };
    };
  }

  # Markdown
  {
    languages = [
      {
        name = "markdown";
        file-types = [
          "md"
          "markdown"
        ];
      }
    ];
    serverNames = [
      "marksman"
      "vscode-markdown-language-server"
    ];
    servers = {
      marksman = {
        command = "marksman";
        args = [ "server" ];
      };
      vscode-markdown-language-server = {
        command = "vscode-markdown-language-server";
        args = [ "--stdio" ];
      };
    };
  }

  # Graphviz
  {
    languages = [
      {
        name = "dot";
        file-types = [
          "dot"
          "gv"
        ];
      }
    ];
    servers = {
      dot-language-server = {
        command = "dot-language-server";
        args = [ "--stdio" ];
      };
    };
  }

  # Uiua
  {
    languages = [
      {
        name = "uiua";
        file-types = [ "ua" ];
      }
    ];
    servers = {
      uiua = {
        command = "uiua";
        args = [ "lsp" ];
      };
    };
  }

  # Blueprint
  {
    languages = [
      {
        name = "blueprint";
        file-types = [ "blp" ];
      }
    ];
    servers = {
      blueprint-compiler = {
        command = "blueprint-compiler";
        args = [ "lsp" ];
      };
    };
  }

  # Odin
  {
    languages = [
      {
        name = "odin";
        file-types = [ "odin" ];
      }
    ];
    servers = {
      ols = {
        command = "ols";
      };
    };
  }
])
