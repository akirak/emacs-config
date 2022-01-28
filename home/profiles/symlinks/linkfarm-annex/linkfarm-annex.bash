#!/usr/bin/env bash

set -euo pipefail

root="/git-annex"

r=0

for target in $(find "$root" -maxdepth 2 -mindepth 2 -type d); do
  if [[ ! -d "$target/.git" ]]; then
    continue
  fi

  name=$(basename "$target")
  src="$HOME/$name"
  if [[ -L "$src" ]]; then
    if [[ $(readlink "$src") = "$target" ]]; then
      echo "OK: $src -> $target" >&2
    else
      echo "Error: point to a different location: $src -> $target" >&2
      r=1
    fi
  elif [[ -e "$src" ]]; then
    echo "Error: not a symlink: $src" >&2
    r=1
  else
    ln -v -s "$target" "$src"
  fi
done

exit $r
