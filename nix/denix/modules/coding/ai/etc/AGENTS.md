# Global AGENTS.md

## Working agreements

- `python` interpreter is generally unavailable. For trivial tasks, use
  command-line tools such as `bash`, `grep`, `sed`, and `rg`.
- Don't try to create new files in `~/.agents` and `~/.codex`; They are supposed
  to be read-only. Unless specifically noted, new skills are project-local, so
  create them in `.agents/skills` in the project directory.
