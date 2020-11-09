#!/usr/bin/env bash

set -e; # will make the script stop if there are any errors

# Actual tests (using the test suite)
for f in ./bench/*.carp; do
    echo $f
    ./scripts/carp.sh -x --optimize $f
    echo
done
