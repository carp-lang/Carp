#!/usr/bin/env sh

# Runs the executable and compares its output to the .expected file
./scripts/carp.sh $1 --log-memory -b --eval-preload '(Project.config "output-directory" "out")' && \
  ./out/Untitled > test/output/$1.output.actual 2>&1
echo $1

if ! diff --strip-trailing-cr test/output/$1.output.actual test/output/$1.output.expected; then
  echo "$1 failed."
  exit 1
else
  rm test/output/$1.output.actual
fi
