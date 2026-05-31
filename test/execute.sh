#!/usr/bin/env sh

# Runs the executable and compares its output to the .expected file
if [ "${CARP_PARALLEL:-0}" = "1" ]; then
  OUT="out/par/$$-$(basename "$1" .carp)"
  mkdir -p "$OUT"
  trap 'rm -rf "$OUT"' EXIT
  ./scripts/carp.sh --eval-preload "(Project.config \"output-directory\" \"$OUT\")" "$1" --log-memory -b --no-profile && \
    "$OUT/Untitled" > test/output/$1.output.actual 2>&1
else
  export CARP_OPTS="$CARP_OPTS  $1 --log-memory -b --no-profile"
  ./scripts/carp.sh && \
    ./out/Untitled > test/output/$1.output.actual 2>&1
fi
echo $1

if ! diff --strip-trailing-cr test/output/$1.output.actual test/output/$1.output.expected; then
  echo "$1 failed."
  exit 1
else
  rm test/output/$1.output.actual
fi
