#!/usr/bin/env bash

set -e; # will make the script stop if there are any errors
set -u; # will make the script stop if there is use of undefined

NO_SDL=0
if [[ $# -gt 0 ]] && [[ "$1" == "--no_sdl" ]]; then
    NO_SDL=1
fi

./build.sh

# Build and run some examples
./test/execute.sh ./examples/basics.carp
./test/execute.sh ./examples/functor.carp
./test/execute.sh ./examples/external_struct.carp
./test/execute.sh ./examples/updating.carp
./test/execute.sh ./examples/sorting.carp
./test/execute.sh ./examples/globals.carp
./test/execute.sh ./examples/generic_structs.carp
./test/execute.sh ./examples/setting_variables.carp
./test/execute.sh ./examples/function_members.carp
./test/execute.sh ./examples/maps.carp
./test/execute.sh ./examples/lambdas.carp
./test/execute.sh ./examples/sumtypes.carp

# Actual tests (using the test suite)
for f in ./test/*.carp; do
    echo $f
   ./carp.sh -x --log-memory $f
    echo
done

# Test for correct error messages when doing "carp --check" on the source.
for f in ./test-for-errors/*.carp; do
    echo $f
   ./test/check.sh $f
    echo
done

# Just make sure these compile
./carp.sh ./examples/mutual_recursion.carp -b
./carp.sh ./examples/guessing.carp -b
./carp.sh ./examples/no_core.carp --no-core --no-profile -b
./carp.sh ./examples/check_malloc.carp -b

# Run tests which rely on SDL unless the `--no_sdl` argument was passed in
if [[ ${NO_SDL} -eq 0 ]]; then
    ./carp.sh ./examples/ant.carp -b
    ./carp.sh ./examples/reptile.carp -b
    ./carp.sh ./examples/game.carp -b
    ./carp.sh ./examples/minimal_sdl.carp -b
    ./carp.sh examples/sounds.carp -b
    ./carp.sh examples/fonts.carp -b
fi

# Generate docs
./carp.sh ./docs/core/generate_core_docs.carp
./carp.sh ./docs/sdl/generate_sdl_docs.carp

echo "ALL TESTS DONE."
