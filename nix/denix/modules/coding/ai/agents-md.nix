{
  delib,
  pkgs,
  lib,
  ...
}:
delib.module {
  name = "agents-md";

  options =
    with delib;
    let
      contentOption = submoduleOption {
        options = {
          documentTitle = strOption "Global AGENTS.md";
          introduction = allowNull (strOption null);
          sections = attrsOfOption (submodule {
            options = {
              title = noNullDefault (strOption null);
              contents = listOfOption str [ ];
            };
          }) { };
        };
      };
    in
    moduleOptions {
      enable = boolOption false;

      defaultContent = contentOption {
        sections = {
          working-agreements = {
            title = "Working agreements";
            contents = [
              ''
                - `python` interpreter is generally unavailable. For trivial tasks, use
                  command-line tools such as `bash`, `grep`, `sed`, and `rg`.
                - Don't try to create new files in `~/.agents` and `~/.codex`; They are supposed
                  to be read-only. Unless specifically noted, new skills are project-local, so
                  create them in `.agents/skills` in the project directory.
              ''
            ];
          };
        };
      };

      files = attrsOfOption (submodule {
        options = {
          pathFromHome = noNullDefault (strOption null);
          content = allowNull (contentOption null);
        };
      }) { };
    };

  home.ifEnabled =
    { cfg, ... }:
    let
      writeMarkdown =
        filename: contentDef:
        pkgs.runCommand filename
          {
            buildInputs = [
              pkgs.mdformat
              pkgs.gitleaks
            ];
            passAsFile = [ "content" ];

            content = ''
              # ${contentDef.documentTitle}
              ${lib.optionalString (contentDef.introduction != null) contentDef.introduction}

              ${lib.concatMapAttrsStringSep "\n\n" (_: section: ''
                ## ${section.title}
                ${lib.concatStringsSep "\n\n" section.contents}
              '') contentDef.sections}
            '';
          }
          ''
            gitleaks stdin < $contentPath
            mdformat - < $contentPath > $out
          '';
    in
    {
      home.file = lib.mapAttrs' (
        _: fileDef:
        lib.nameValuePair fileDef.pathFromHome {
          source = writeMarkdown "AGENTS.md" (
            if fileDef.content != null then fileDef.content else cfg.defaultContent
          );
        }
      ) cfg.files;
    };
}
