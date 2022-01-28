#!/usr/bin/env bash

set -euo pipefail

if [[ "$#" -eq 0 ]]; then
  echo "Usage: $0 DIR"
fi

root="$1"

if [[ ! -d "$root" ]]; then
  echo "$root does not exist." >&2
  exit 1
fi

for target in $(find "$root" -maxdepth 2 -mindepth 2 -type d); do
  path=${target##$root/}
  parent="$HOME/$(dirname $path)"
  mkdir -p "$parent"
  src="$parent/$(basename $target)"
  if [[ -L "$src" ]]; then
      echo "OK: $src"
  elif [[ -e "$src" ]]; then
      echo "Error: not a symlink: $src" >&2
  else
      ln -v -t "$parent" -s "$target"
  fi
done
