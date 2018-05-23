#!/bin/bash

set -e; # will make the script stop if there are any errors
set -u; # will make the script stop if there is use of undefined

NO_SDL=0
if [[ $# -gt 0 ]] && [[ "$1" == "--no_sdl" ]]; then
    NO_SDL=1
fi

stack build;
# This is useful for devs, but these tests should not rely on `carp` being in
# PATH.
stack install;

# Build and run some examples
./test/check.sh ./examples/basics.carp
./test/check.sh ./examples/functor.carp
./test/check.sh ./examples/external_struct.carp
./test/check.sh ./examples/updating.carp
./test/check.sh ./examples/sorting.carp
./test/check.sh ./examples/globals.carp
./test/check.sh ./examples/generic_structs.carp
./test/check.sh ./examples/setting_variables.carp
./test/check.sh ./examples/function_members.carp

# Actual tests (using the test suite)
for f in ./test/*.carp; do
    echo $f
    stack exec carp -- -x --log-memory $f
    echo
done

# Just make sure these compile
stack exec carp -- ./examples/mutual_recursion.carp -b
stack exec carp -- ./examples/guessing.carp -b
stack exec carp -- ./examples/no_core.carp --no-core -b

# Run tests which rely on SDL unless the `--no_sdl` argument was passed in
if [[ ${NO_SDL} -eq 0 ]]; then
    stack exec carp -- ./examples/ant.carp -b
    stack exec carp -- ./examples/reptile.carp -b
    stack exec carp -- ./examples/game.carp -b
    stack exec carp -- ./examples/minimal_sdl.carp -b
    stack exec carp -- examples/sounds.carp -b
    stack exec carp -- examples/fonts.carp -b
fi

# Generate core docs
stack exec carp -- ./docs/core/generate_core_docs.carp -b

echo "ALL TESTS DONE."
