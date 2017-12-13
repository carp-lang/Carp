#!/bin/bash

set -e; # will make the script stop if there are any errors

stack build;
stack install;

carp ./examples/basics.carp;
carp ./examples/deleters.carp;
carp ./examples/copying.carp;
carp ./examples/polymorphic.carp;
carp ./examples/selection.carp;
carp ./examples/functor.carp;
carp ./examples/vec2.carp;
carp ./examples/array.carp;
carp ./examples/updating.carp;
carp ./examples/external_struct.carp;
carp ./examples/strings.carp -x;
carp ./examples/interfaces.carp -x;

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

carp ./examples/game.carp -b;

echo "ALL TESTS DONE."
