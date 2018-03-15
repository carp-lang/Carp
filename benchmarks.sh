#!/bin/bash

set -e; # will make the script stop if there are any errors

stack build;
stack install;

# Actual tests (using the test suite)
for f in ./bench/*.carp; do
    echo $f
    carp -x --optimize $f
    echo
done
