#!/bin/sh
set -eu

repo_root=$(git rev-parse --show-toplevel)
hooks_folder="$repo_root/.git/hooks"

ln -sf "$repo_root/scripts/git-hooks/commit-msg.sh" "$hooks_folder/commit-msg"

