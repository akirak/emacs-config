{ lib
, writeShellScriptBin
, emacs-config
}:
with builtins;
let
  emacsBatch = "${emacs-config}/bin/emacs --batch ${
    lib.escapeShellArgs (lib.flatten (map (file: [ "-l" file ]) emacs-config.initFiles))
  }";
in
writeShellScriptBin "test-emacs-config" ''
  set -euo pipefail
  ${emacsBatch} -l org-capture -f el-patch-validate-all
''
