#!/bin/bash

set -e; # will make the script stop if there are any errors

stack build;
stack install;

# Build and run some examples
carp ./examples/basics.carp -x;
carp ./examples/functor.carp -x;
carp ./examples/external_struct.carp -x;
carp ./examples/updating.carp -x;
carp ./examples/globals.carp -x --log-memory;

# Actual tests (using the test suit)
carp ./test/memory.carp -x --log-memory;
carp ./test/double_math.carp -b; ./out/a.out;
carp ./test/float_math.carp -b; ./out/a.out;
carp ./test/int_math.carp -b; ./out/a.out;
carp ./test/long_math.carp -b; ./out/a.out;
carp ./test/safe_artihmetic.carp -b; ./out/a.out;
carp ./test/statistics.carp -b; ./out/a.out;
carp ./test/vector2.carp -b; ./out/a.out;
carp ./test/vector3.carp -b; ./out/a.out;
carp ./test/vectorn.carp -b; ./out/a.out;
carp ./test/control_flow.carp -b; ./out/a.out;
carp ./test/macros.carp -b; ./out/a.out;

# Just make sure these compile
carp ./examples/mutual_recursion.carp -b;
carp ./examples/guessing.carp -b;
carp ./examples/game.carp -b;

echo "ALL TESTS DONE."
