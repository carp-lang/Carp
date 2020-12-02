#!/bin/sh
set -eu

check_ormolu_installed() {
  if ! command -v ormolu >/dev/null 2>&1
  then
    echo "'ormolu' is needed to format Haskell files"
    echo "On MacOS it is available in brew: 'brew install ormolu'"
    echo "Otherwise it can be installed using stack: 'stack install ormolu'"
    echo "The last command needs to be run outside of the repository"
    exit 1
  fi
}

repo_root=$(git rev-parse --show-toplevel)

if [ "${1-}" = "--only-changed" ]; then
  cd "$repo_root"
  changed_haskell_files="$(git diff --relative --name-only head | sed '/.*\.hs$/!d')"
  if [ "$changed_haskell_files" = '' ]; then
    exit 0
  fi
  check_ormolu_installed
  ormolu --mode inplace $(git diff --relative --name-only head | sed '/.*\.hs$/!d')
else
  check_ormolu_installed
  ormolu --mode inplace $(find $repo_root -name '*.hs')
fi

