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

for owner in ${owners[@]}
do
    jq -r ".nodes | to_entries | map(select(.value.original.owner == \"$owner\")) | map(.key) | .[]" \
       flake.lock
done | nix flake update
