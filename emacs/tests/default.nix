{ lib
, runCommandLocal
, emacs-config
}:
with builtins;
let
  emacsBatch = "emacs --batch ${
    lib.escapeShellArgs (lib.flatten
      (map (file: [ "-l" file ]) emacs-config.initFiles))
  }";
in
runCommandLocal "test-emacs-config"
{
  buildInputs = [ emacs-config ];
} ''
  set -euo pipefail
  ${emacsBatch}
  touch $out
''
