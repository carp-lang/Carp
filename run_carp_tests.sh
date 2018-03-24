#!/bin/bash

set -e; # will make the script stop if there are any errors

stack build;
stack install;

# Build and run some examples
carp ./examples/basics.carp -x;
carp ./examples/functor.carp -x;
carp ./examples/external_struct.carp -x;
carp ./examples/updating.carp -x;
carp ./examples/sorting.carp -x;
carp ./examples/globals.carp -x --log-memory;
carp ./examples/generic_structs.carp -x;
carp ./examples/setting_variables.carp -x;
carp ./examples/function_members.carp -x;

# Actual tests (using the test suite)
for f in ./test/*.carp; do
    echo $f
    carp -x --log-memory $f
    echo
done

# Just make sure these compile
carp ./examples/mutual_recursion.carp -b;
carp ./examples/guessing.carp -b;
carp ./examples/ant.carp -b
carp ./examples/reptile.carp -b
carp ./examples/game.carp -b;
carp ./examples/minimal_sdl.carp -b;
carp examples/sounds.carp -b
carp examples/fonts.carp -b
carp ./examples/no_core.carp --no-core -b;

echo "ALL TESTS DONE."
