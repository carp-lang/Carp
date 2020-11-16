#!/usr/bin/env sh

# Checks the code (using --check) and compares the output to the .expected file
./scripts/carp.sh $1 --log-memory --check > test/output/$1.output.actual 2>&1

if ! diff --strip-trailing-cr test/output/$1.output.actual test/output/$1.output.expected; then
  echo "$1 failed."
  exit 1
else
  rm test/output/$1.output.actual
fi
