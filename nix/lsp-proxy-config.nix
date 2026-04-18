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
            # Use copilot as the default language server.
            language-servers = builtins.attrNames a.servers ++ [ "copilot" ];
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
    servers = {
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
    servers = {
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
    # The Emacs config conditionally adds a debug bundle to jdtls when the jar
    # exists in the user's Emacs directory. That dynamic path is intentionally
    # left out here.
    servers = {
      jdtls = {
        command = "jdtls";
      };
    };
  }
])
