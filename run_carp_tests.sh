#!/bin/bash

stack build;

stack exec Carp ./examples/basics.carp;
stack exec Carp ./examples/deleters.carp;
stack exec Carp ./examples/copying.carp;
stack exec Carp ./examples/polymorphic.carp;
stack exec Carp ./examples/selection.carp;
stack exec Carp ./examples/functor.carp;
stack exec Carp ./examples/vec2.carp;
stack exec Carp ./examples/array.carp;
stack exec Carp ./examples/updating.carp;
stack exec Carp ./examples/external_struct.carp;

# Game will run until closed:
stack exec Carp ./examples/game.carp;
