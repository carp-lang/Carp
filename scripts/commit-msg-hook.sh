#!/bin/sh
set -euo noglob

VALID_TYPES="build|ci|chore|docs|feat|fix|perf|refactor|revert|style|test"
COMMIT_MESSAGE=$(cat "$1")
OUTPUT=$(echo "$COMMIT_MESSAGE" | sed -n 1p | sed -E "s/^($VALID_TYPES)(\(.*\))?(\!|\\\!)?: .*//");
if [ ${#OUTPUT} -eq 0 ] && [ ${#COMMIT_MESSAGE} -ne 0 ]; then
  exit 0;
else
  echo "Commit message does not follow Conventional Commits."
  echo "Examples:"
  echo "  fix: Fixes a bug"
  echo "  feat!: Adds a feature with breaking change"
  echo "The following types are allowed: $VALID_TYPES"
  echo "For more information see https://www.conventionalcommits.org"
  exit 1;
fi

