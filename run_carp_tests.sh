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
#carp ./examples/generic_structs.carp -x;

# Actual tests (using the test suite)
carp ./test/memory.carp -x --log-memory;
for f in ./test/*.carp; do
  if [ $f != "./test/memory.carp" ]; then
    carp -x $f
  fi
done

# Just make sure these compile
carp ./examples/mutual_recursion.carp -b;
carp ./examples/guessing.carp -b;
carp ./examples/game.carp -b;

echo "ALL TESTS DONE."
