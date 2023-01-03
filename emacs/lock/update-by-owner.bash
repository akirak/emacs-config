#!/usr/bin/env bash

set -euo pipefail

if [[ $# -gt 0 ]]
then
    owners=("$@")
else
    echo "Usage: update-by-owner OWNER.."
    exit 1
fi

declare -a args

tmp=$(mktemp)
trap "rm $tmp" ERR EXIT

for owner in ${owners[@]}
do
    jq -r ".nodes | to_entries | map(select(.value.original.owner == \"$owner\")) | map(.key) | .[]" \
       flake.lock | sed -e 's/^/--update-input /' >> "$tmp"
done

xargs nix flake lock < "$tmp"
