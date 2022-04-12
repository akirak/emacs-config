#!/usr/bin/env bash

set -euo pipefail

if [[ $# -gt 0 ]]
then
    owners=("$@")
else
    # I always trust myself.
    owners=(akirak emacs-twist)
fi

declare -a args

tmp=$(mktemp)
trap "rm $tmp" ERR EXIT

for owner in ${owners[@]}
do
    jq -r ".nodes | to_entries | map(select(.value.original.type == \"github\" and .value.original.owner == \"$owner\")) | map(.key) | .[]" flake.lock |
        sed -e 's/^/--update-input /' >> "$tmp"
done

echo "--update-input akirak" >> "$tmp"

xargs nix flake lock < "$tmp"
