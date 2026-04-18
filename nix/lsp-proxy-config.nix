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
            language-servers = [ "copilot" ] ++ builtins.attrNames a.servers;
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
])
