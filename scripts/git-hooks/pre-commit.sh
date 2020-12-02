#!/bin/sh
set -eu

repo_root=$(git rev-parse --show-toplevel)

changed_haskell_files=$("$repo_root/scripts/format.sh" --only-changed)
[ -z "$changed_haskell_files" ] && exit 0

echo $changed_haskell_files
git add $changed_haskell_files
