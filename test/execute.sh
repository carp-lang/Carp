#!/usr/bin/env sh

# Runs the executable and compares its output to the .expected file
export CARP_OPTS="$CARP_OPTS  $1 --log-memory -b --no-profile"
./scripts/carp.sh && \
  ./out/Untitled > test/output/$1.output.actual 2>&1
echo $1

if ! diff --strip-trailing-cr test/output/$1.output.actual test/output/$1.output.expected; then
  echo "$1 failed."
  exit 1
else
  rm test/output/$1.output.actual
fi
