#!/bin/bash
# Evaluator benchmark runner
#
# Measures Haskell-side compiler/evaluator performance.
# Uses --generate-only to skip C compilation and linking.
#
# Usage:
#   ./bench/run-evaluator-bench.sh          # default 5 runs
#   ./bench/run-evaluator-bench.sh 10       # 10 runs

set -e

RUNS=${1:-5}
CARP="stack exec carp --"
BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$BENCH_DIR")"

cd "$PROJECT_DIR"

echo "Evaluator Benchmark"
echo "==================="
echo "Runs: $RUNS"
echo ""

run_bench() {
    local label="$1"
    local file="$2"
    local times=()

    echo "--- $label ---"
    echo "File: $file"

    for i in $(seq 1 "$RUNS"); do
        # Use bash's TIMEFORMAT to get just wall-clock seconds
        t=$( { TIMEFORMAT='%R'; time $CARP --generate-only -b "$file" > /dev/null 2>&1; } 2>&1 )
        times+=("$t")
        printf "  Run %d: %ss\n" "$i" "$t"
    done

    # Compute min, max, median using sort
    sorted=($(printf '%s\n' "${times[@]}" | sort -n))
    local n=${#sorted[@]}
    local mid=$((n / 2))

    echo "  Min:    ${sorted[0]}s"
    echo "  Max:    ${sorted[$((n-1))]}s"
    echo "  Median: ${sorted[$mid]}s"
    echo ""
}

# Baseline: minimal program (measures startup overhead)
run_bench "Baseline (startup)" "bench/baseline.carp"

# Main evaluator benchmark
run_bench "Evaluator benchmark" "bench/evaluator.carp"

# Real-world: test/macros.carp exercises many macro patterns
run_bench "Real-world (test/macros.carp)" "test/macros.carp"
