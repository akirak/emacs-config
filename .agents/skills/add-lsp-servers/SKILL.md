---
name: add-lsp-servers
description: Add or update `lsp-proxy` language server entries from Eglot defaults. Use when Codex needs to inspect `eglot-server-programs` in `emacs -Q --batch`, compare the built-in defaults against `nix/lsp-proxy-config.nix`, and add missing command-backed language or server mappings without relying on user init files.
---

# Add LSP Server

## Overview

Use Eglot's built-in defaults as the source of truth, then mirror missing command-backed entries into `nix/lsp-proxy-config.nix`.

## Workflow

1. Run `scripts/print-default-eglot-servers.sh`.
2. Compare the printed `eglot-server-programs` value against `nix/lsp-proxy-config.nix`.
3. Reuse an existing language block when the language is already present. Add only the missing server entries or file types.
4. When the Eglot default lists multiple server candidates, preserve that preference order directly in the `servers` list.
5. Add only entries that map cleanly to executable commands and arguments. Skip host-and-port transports or entries that depend on dynamic project-path arguments unless the target schema already supports them.
6. Use conservative file type mappings. Prefer obvious extensions and explicit globs such as `Dockerfile` or `go.mod`.
7. Format the file with `nix fmt nix/lsp-proxy-config.nix`. Run `nix build .#checks.SYSTEM.lsp-proxy -L` to check if the config successfully builds. `SYSTEM` should be `x86_64-linux` on typical Linux systems.

## Notes

- Run Eglot without any init file:

```bash
emacs -Q --batch --eval "(require 'eglot)" --eval "(prin1 eglot-server-programs)"
```

- `eglot-alternatives` entries print as byte-code objects. The candidate command list still appears in the printed constant vector, so the raw output is enough to recover executable names and arguments.
- When Eglot offers several alternatives, model them as separate `servers` entries and keep them in preference order.
- Prefer extending the helper that builds `language-servers` over duplicating ordering logic in every block.
