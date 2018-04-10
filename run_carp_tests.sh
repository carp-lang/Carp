#!/bin/bash

set -e; # will make the script stop if there are any errors

stack build;
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
    carp -x --log-memory $f
    echo
done

# Just make sure these compile
carp ./examples/mutual_recursion.carp -b
carp ./examples/guessing.carp -b
carp ./examples/ant.carp -b
carp ./examples/reptile.carp -b
carp ./examples/game.carp -b
carp ./examples/minimal_sdl.carp -b
carp examples/sounds.carp -b
carp examples/fonts.carp -b
carp ./examples/no_core.carp --no-core -b

# Generate core docs
carp ./docs/core/generate_core_docs.carp -b

echo "ALL TESTS DONE."
