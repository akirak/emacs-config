#!/usr/bin/env bash

set -euo pipefail

trusted_owners=(akirak emacs-twist)
declare -a args

tmp=$(mktemp)
trap "rm $tmp" ERR EXIT

for owner in ${trusted_owners[@]}
do
    jq -r ".nodes | to_entries | map(select(.value.original.type == \"github\" and .value.original.owner == \"$owner\")) | map(.key) | .[]" flake.lock |
        sed -e 's/^/--update-input /' >> "$tmp"
done

xargs nix flake lock "$@" < "$tmp"
