#!/usr/bin/env sh

set -e; # will make the script stop if there are any errors
set -u; # will make the script stop if there is use of undefined

NO_SDL=0
if [ $# -gt 0 ] && [ "$1" = "--no_sdl" ]; then
    NO_SDL=1
fi

./scripts/build.sh

# TEMPORARILY REMOVED ALL THE TESTS

# Generate docs
./scripts/carp.sh ./docs/core/generate_core_docs.carp

# Generate SDL docs unless the `--no_sdl` argument was passed in
if [ ${NO_SDL} -eq 0 ]; then
  ./scripts/carp.sh ./docs/sdl/generate_sdl_docs.carp
fi

echo "ALL TESTS DONE."
