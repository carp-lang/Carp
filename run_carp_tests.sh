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

carp ./test/double_math.carp -x;
carp ./test/float_math.carp -x;
carp ./test/int_math.carp -x;
carp ./test/safe_artihmetic.carp -x;
carp ./test/statistics.carp -x;
carp ./test/vector2.carp -x;
carp ./test/vector3.carp -x;
carp ./test/vectorn.carp -x;

carp ./examples/game.carp -b;
