#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNS="${1:-9}"
OUT_MAIN="$ROOT_DIR/out/main.c"

CASES=(
  "test/macros.carp"
  "test/regression.carp"
  "test/string.carp"
  "test/map.carp"
  "test/memory.carp"
)

summarize_times() {
  local file="$1"
  local sorted count min max median
  sorted="$(LC_ALL=C sort -n "$file")"
  count="$(printf '%s\n' "$sorted" | awk 'NF{n++} END{print n}')"
  min="$(printf '%s\n' "$sorted" | awk 'NF{print; exit}')"
  max="$(printf '%s\n' "$sorted" | awk 'NF{v=$0} END{print v}')"
  if (( count % 2 == 1 )); then
    median="$(printf '%s\n' "$sorted" | awk -v idx=$((count / 2 + 1)) 'NF && NR==idx {print; exit}')"
  else
    median="$(printf '%s\n' "$sorted" | awk -v i1=$((count / 2)) -v i2=$((count / 2 + 1)) 'NF && NR==i1 {a=$1} NF && NR==i2 {b=$1} END {printf "%.6f", (a+b)/2}')"
  fi
  printf '%s|%s|%s' "$min" "$max" "$median"
}

(
  cd "$ROOT_DIR"
  CARP_DIR="$ROOT_DIR"
  export CARP_DIR
  stack build >/tmp/carp-emitter-prebuild.out 2>&1
)

for case_path in "${CASES[@]}"; do
  echo "--- $case_path ---"
  (
    cd "$ROOT_DIR"
    CARP_DIR="$ROOT_DIR"
    export CARP_DIR
    stack exec carp -- --generate-only -b "$case_path" >/tmp/carp-emitter-bench.out 2>&1
  )

  times_file="$(mktemp)"
  for i in $(seq 1 "$RUNS"); do
    elapsed="$(
      cd "$ROOT_DIR"
      CARP_DIR="$ROOT_DIR"
      export CARP_DIR
      {
        TIMEFORMAT=%R
        time stack exec carp -- --generate-only -b "$case_path" >/tmp/carp-emitter-bench.out 2>&1
      } 2>&1
    )"
    printf '%s\n' "$elapsed" >> "$times_file"
    echo "  Run $i: ${elapsed}s"
  done

  size_bytes="n/a"
  if [[ -f "$OUT_MAIN" ]]; then
    size_bytes="$(wc -c < "$OUT_MAIN" | tr -d ' ')"
  fi

  summary="$(summarize_times "$times_file")"
  rm -f "$times_file"
  min_s="${summary%%|*}"
  rest="${summary#*|}"
  max_s="${rest%%|*}"
  median_s="${rest#*|}"

  echo "  C size:  $size_bytes"
  echo "  Min:     ${min_s}s"
  echo "  Max:     ${max_s}s"
  echo "  Median:  ${median_s}s"
  echo
done
