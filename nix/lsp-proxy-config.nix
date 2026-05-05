# SPDX-License-Identifier: GPL-3.0-or-later
# Assisted-by: codex:gpt-5.4
#
# A configuration for lsp-proxy in Nix.
#
# This configuration takes an inspiration from https://apribase.net/2026/03/09/lsp-proxy/.
# See https://github.com/jadestrong/lsp-proxy#language-configuration for the
# configuration schema.
#
# The server configurations are imported from eglot.el, so this file is licensed
# under GPL.
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

  serverName = server: server.name or server.command;

  serverDefinition =
    server: lib.nameValuePair (serverName server) (builtins.removeAttrs server [ "name" ]);

  fromEglotStyleSettings = eglotStyleSettings: {
    language-server =
      defaultLanguageServers
      // (lib.foldl' (
        acc: a: acc // (builtins.listToAttrs (builtins.map serverDefinition a.servers))
      ) { } eglotStyleSettings);

    language = lib.flatten (
      builtins.map (
        a:
        (builtins.map (
          language:
          language
          // {
            language-servers = (builtins.map serverName a.servers) ++ [ "copilot" ];
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
    servers = [
      {
        command = "oxlint";
        args = [
          "--lsp"
        ];
      }
      {
        command = "tsgo";
        args = [
          "--lsp"
          "--stdio"
        ];
      }
      {
        command = "typescript-language-server";
        args = [
          "--stdio"
        ];
      }
      {
        command = "deno";
        args = [
          "lsp"
        ];
      }
      {
        name = "rass-ts";
        command = "rass";
        args = [ "ts" ];
      }
    ];
  }

  # Nix
  {
    languages = [
      {
        name = "nix";
        file-types = [ "nix" ];
      }
    ];
    servers = [
      {
        command = "tix";
        args = [ "lsp" ];
      }
      {
        command = "typenix";
        args = [
          "--lsp"
          "--stdio"
        ];
      }
      {
        command = "nil";
      }
      {
        command = "rnix-lsp";
      }
      {
        command = "nixd";
      }
    ];
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
    servers = [
      {
        command = "just-lsp";
      }
    ];
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
    servers = [
      {
        command = "lake";
        args = [ "serve" ];
      }
    ];
  }

  # SQL
  {
    languages = [
      {
        name = "sql";
        file-types = [ "sql" ];
      }
    ];
    servers = [
      {
        command = "sqlmesh_lsp";
      }
    ];
  }

  # Svelte
  {
    languages = [
      {
        name = "svelte";
        file-types = [ "svelte" ];
      }
    ];
    servers = [
      {
        command = "svelteserver";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "terraform-ls";
        args = [ "serve" ];
      }
      {
        command = "terraform-lsp";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "ocamllsp";
      }
    ];
  }

  # Coq (Rocq)
  {
    languages = [
      {
        name = "coq";
        file-types = [ "v" ];
      }
    ];
    servers = [
      {
        command = "coq-lsp";
      }
    ];
  }

  # Gleam
  {
    languages = [
      {
        name = "gleam";
        file-types = [ "gleam" ];
      }
    ];
    servers = [
      {
        command = "gleam";
        args = [ "lsp" ];
      }
    ];
  }

  # Zig
  {
    languages = [
      {
        name = "zig";
        file-types = [ "zig" ];
      }
    ];
    servers = [
      {
        command = "zls";
      }
    ];
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
    servers = [
      {
        command = "fsautocomplete";
        args = [ "--adaptive-lsp-server-enabled" ];
      }
    ];
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
    servers = [
      {
        command = "lexical";
      }
      {
        command = "nextls";
        args = [ "--stdio=true" ];
      }
      {
        command = "elixir-ls";
      }
    ];
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
    servers = [
      {
        command = "astro-ls";
        args = [ "--stdio" ];
      }
    ];
  }

  # Java
  {
    languages = [
      {
        name = "java";
        file-types = [ "java" ];
      }
    ];
    # The Emacs config conditionally adds a debug bundle to jdtls when the jar
    # exists in the user's Emacs directory. That dynamic path is intentionally
    # left out here.
    servers = [
      {
        command = "jdtls";
      }
      {
        command = "java-language-server";
      }
    ];
  }

  # Rust
  {
    languages = [
      {
        name = "rust";
        file-types = [ "rs" ];
      }
    ];
    servers = [
      {
        command = "rust-analyzer";
      }
    ];
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
    servers = [
      {
        command = "neocmakelsp";
        args = [ "stdio" ];
      }
      {
        command = "cmake-language-server";
      }
    ];
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
    servers = [
      {
        command = "vim-language-server";
        args = [ "--stdio" ];
      }
    ];
  }

  # Python
  {
    languages = [
      {
        name = "python";
        file-types = [ "py" ];
      }
    ];
    servers = [
      {
        name = "rass-python";
        command = "rass";
        args = [ "python" ];
      }
      {
        command = "pylsp";
      }
      {
        command = "pyls";
      }
      {
        command = "basedpyright-langserver";
        args = [ "--stdio" ];
      }
      {
        command = "pyright-langserver";
        args = [ "--stdio" ];
      }
      {
        command = "pyrefly";
        args = [ "lsp" ];
      }
      {
        command = "ty";
        args = [ "server" ];
      }
      {
        command = "jedi-language-server";
      }
      {
        command = "ruff";
        args = [ "server" ];
      }
      {
        command = "ruff-lsp";
      }
    ];
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
    servers = [
      {
        command = "vscode-json-language-server";
        args = [ "--stdio" ];
      }
      {
        command = "vscode-json-languageserver";
        args = [ "--stdio" ];
      }
      {
        command = "json-languageserver";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "bash-language-server";
        args = [ "start" ];
      }
    ];
  }

  # PHP
  {
    languages = [
      {
        name = "php";
        file-types = [ "php" ];
      }
    ];
    servers = [
      {
        command = "phpactor";
        args = [ "language-server" ];
      }
      {
        name = "php-language-server";
        command = "php";
        args = [ "vendor/felixfbecker/language-server/bin/php-language-server.php" ];
      }
    ];
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
    servers = [
      {
        command = "clangd";
      }
      {
        command = "ccls";
      }
    ];
  }

  # Ruby
  {
    languages = [
      {
        name = "ruby";
        file-types = [ "rb" ];
      }
    ];
    servers = [
      {
        command = "ruby-lsp";
      }
    ];
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
    servers = [
      {
        command = "haskell-language-server-wrapper";
        args = [ "--lsp" ];
      }
      {
        command = "static-ls";
      }
    ];
  }

  # Elm
  {
    languages = [
      {
        name = "elm";
        file-types = [ "elm" ];
      }
    ];
    servers = [
      {
        command = "elm-language-server";
      }
    ];
  }

  # Mint
  {
    languages = [
      {
        name = "mint";
        file-types = [ "mint" ];
      }
    ];
    servers = [
      {
        command = "mint";
        args = [ "ls" ];
      }
    ];
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
    servers = [
      {
        command = "kotlin-language-server";
      }
    ];
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
    servers = [
      {
        command = "gopls";
      }
    ];
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
    servers = [
      {
        name = "r-languageserver";
        command = "R";
        args = [
          "--slave"
          "-e"
          "languageserver::run()"
        ];
      }
    ];
  }

  # Dart
  {
    languages = [
      {
        name = "dart";
        file-types = [ "dart" ];
      }
    ];
    servers = [
      {
        command = "dart";
        args = [
          "language-server"
          "--client-id"
          "emacs.eglot-dart"
        ];
      }
    ];
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
    servers = [
      {
        name = "ada-language-server";
        command = "ada_language_server";
      }
    ];
  }

  # GPR
  {
    languages = [
      {
        name = "gpr";
        file-types = [ "gpr" ];
      }
    ];
    servers = [
      {
        name = "ada-language-server-gpr";
        command = "ada_language_server";
        args = [ "--language-gpr" ];
      }
    ];
  }

  # Scala
  {
    languages = [
      {
        name = "scala";
        file-types = [ "scala" ];
      }
    ];
    servers = [
      {
        command = "metals";
      }
      {
        command = "metals-emacs";
      }
    ];
  }

  # Racket
  {
    languages = [
      {
        name = "racket";
        file-types = [ "rkt" ];
      }
    ];
    servers = [
      {
        command = "racket";
        args = [
          "-l"
          "racket-langserver"
        ];
      }
    ];
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
    servers = [
      {
        command = "digestif";
      }
      {
        command = "texlab";
      }
    ];
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
    servers = [
      {
        command = "elp";
        args = [ "server" ];
      }
    ];
  }

  # WebAssembly Text
  {
    languages = [
      {
        name = "wat";
        file-types = [ "wat" ];
      }
    ];
    servers = [
      {
        command = "wat_server";
      }
    ];
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
    servers = [
      {
        command = "yaml-language-server";
        args = [ "--stdio" ];
      }
    ];
  }

  # TOML
  {
    languages = [
      {
        name = "toml";
        file-types = [ "toml" ];
      }
    ];
    servers = [
      {
        command = "tombi";
        args = [ "lsp" ];
      }
    ];
  }

  # Nickel
  {
    languages = [
      {
        name = "nickel";
        file-types = [ "ncl" ];
      }
    ];
    servers = [
      {
        command = "nls";
      }
    ];
  }

  # Nushell
  {
    languages = [
      {
        name = "nushell";
        file-types = [ "nu" ];
      }
    ];
    servers = [
      {
        command = "nu";
        args = [ "--lsp" ];
      }
    ];
  }

  # Fennel
  {
    languages = [
      {
        name = "fennel";
        file-types = [ "fnl" ];
      }
    ];
    servers = [
      {
        command = "fennel-ls";
      }
    ];
  }

  # Move
  {
    languages = [
      {
        name = "move";
        file-types = [ "move" ];
      }
    ];
    servers = [
      {
        command = "move-analyzer";
      }
    ];
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
    servers = [
      {
        command = "fortls";
      }
    ];
  }

  # Futhark
  {
    languages = [
      {
        name = "futhark";
        file-types = [ "fut" ];
      }
    ];
    servers = [
      {
        command = "futhark";
        args = [ "lsp" ];
      }
    ];
  }

  # Lua
  {
    languages = [
      {
        name = "lua";
        file-types = [ "lua" ];
      }
    ];
    servers = [
      {
        command = "lua-language-server";
      }
      {
        command = "lua-lsp";
      }
    ];
  }

  # Yang
  {
    languages = [
      {
        name = "yang";
        file-types = [ "yang" ];
      }
    ];
    servers = [
      {
        command = "yang-language-server";
      }
    ];
  }

  # CSS
  {
    languages = [
      {
        name = "css";
        file-types = [ "css" ];
      }
    ];
    servers = [
      {
        command = "vscode-css-language-server";
        args = [ "--stdio" ];
      }
      {
        command = "css-languageserver";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "vscode-html-language-server";
        args = [ "--stdio" ];
      }
      {
        command = "html-languageserver";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "docker-langserver";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "clojure-lsp";
      }
    ];
  }

  # C#
  {
    languages = [
      {
        name = "csharp";
        file-types = [ "cs" ];
      }
    ];
    servers = [
      {
        command = "omnisharp";
        args = [ "-lsp" ];
      }
      {
        command = "OmniSharp";
        args = [ "-lsp" ];
      }
      {
        command = "csharp-ls";
      }
    ];
  }

  # Purescript
  {
    languages = [
      {
        name = "purescript";
        file-types = [ "purs" ];
      }
    ];
    servers = [
      {
        command = "purescript-language-server";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        name = "perl-language-server";
        command = "perl";
        args = [
          "-MPerl::LanguageServer"
          "-e"
          "Perl::LanguageServer::run"
        ];
      }
    ];
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
    servers = [
      {
        command = "marksman";
        args = [ "server" ];
      }
      {
        command = "vscode-markdown-language-server";
        args = [ "--stdio" ];
      }
    ];
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
    servers = [
      {
        command = "dot-language-server";
        args = [ "--stdio" ];
      }
    ];
  }

  # Uiua
  {
    languages = [
      {
        name = "uiua";
        file-types = [ "ua" ];
      }
    ];
    servers = [
      {
        command = "uiua";
        args = [ "lsp" ];
      }
    ];
  }

  # Blueprint
  {
    languages = [
      {
        name = "blueprint";
        file-types = [ "blp" ];
      }
    ];
    servers = [
      {
        command = "blueprint-compiler";
        args = [ "lsp" ];
      }
    ];
  }

  # Odin
  {
    languages = [
      {
        name = "odin";
        file-types = [ "odin" ];
      }
    ];
    servers = [
      {
        command = "ols";
      }
    ];
  }
])
