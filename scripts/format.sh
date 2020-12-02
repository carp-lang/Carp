#!/bin/sh
set -eu

repo_root=$(git rev-parse --show-toplevel)

if [ "${1-}" = "--only-changed" ]; then
  cd "$repo_root"
  ormolu --mode inplace $(git diff --relative --name-only HEAD | sed '/.*\.hs$/!d')
else
  ormolu --mode inplace $(find $repo_root -name '*.hs')
fi

