#!/usr/bin/env bash
set -euo pipefail

emacs -Q --batch \
  --eval "(require 'eglot)" \
  --eval "(prin1 eglot-server-programs)"
