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
  changed_haskell_files=$(git diff --cached --name-only --diff-filter=ACMR "*.hs" | sed 's| |\\ |g')
  [ -z "$changed_haskell_files" ] && exit 0
  check_ormolu_installed
  ormolu --mode inplace $changed_haskell_files
  echo $changed_haskell_files
else
  check_ormolu_installed
  cd "$repo_root"
  ormolu --mode inplace ./**/*.hs
  echo ./**/*.hs
fi

