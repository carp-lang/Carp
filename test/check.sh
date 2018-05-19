#!/bin/sh

stack exec carp -- $1 --log-memory -x > test/output/$1.output.actual 2>&1

if ! diff test/output/$1.output.actual test/output/$1.output.expected; then
  echo "$1 failed."
  exit 1
else
  rm test/output/$1.output.actual
fi
