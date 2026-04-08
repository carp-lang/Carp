#!/usr/bin/env bash
#
# Run the test suite with code coverage and report per-file line coverage
# for core/. Requires gcov (gcc) or llvm-cov (clang/Xcode).
#
# Usage: ./scripts/coverage.sh [test-glob]
#   test-glob  Optional glob for test files (default: ./test/*.carp)
#
# Examples:
#   ./scripts/coverage.sh                    # all tests
#   ./scripts/coverage.sh ./test/array.carp  # single test
#
# The numbers are conservative: for each core file, we take the max
# lines-hit across individual test runs. Real merged coverage would be
# higher since different tests often cover complementary lines.

set -eu

# Resolve gcov command (macOS needs xcrun llvm-cov gcov)
if [ "$(uname -s)" = "Darwin" ]; then
    GCOV="xcrun llvm-cov gcov"
else
    GCOV="gcov"
fi

# Use the repo's carp.sh wrapper
CARP_SH="./scripts/carp.sh"
if [ ! -x "$CARP_SH" ]; then
    echo "Error: run this script from the carp repo root." >&2
    exit 1
fi

TEST_GLOB="${1:-./test/*.carp}"
RAW=$(mktemp)
trap 'rm -f "$RAW"' EXIT

for test_file in $TEST_GLOB; do
    [ -f "$test_file" ] || continue
    rm -rf out/

    if "$CARP_SH" --line-directives \
       --eval-preload '(Project.config "cflag" "--coverage")' \
       -x "$test_file" >/dev/null 2>&1; then

        current_file=""
        $GCOV -r out/*.gcda 2>/dev/null | while IFS= read -r line; do
            case "$line" in
                File\ *)
                    current_file=$(echo "$line" | sed "s/^File '//;s/'$//")
                    ;;
                Lines\ executed:*)
                    if echo "$current_file" | grep -q '^core/.*\.carp$'; then
                        pct=$(echo "$line" | sed 's/.*:\(.*\)% of.*/\1/')
                        total=$(echo "$line" | sed 's/.*of \([0-9]*\).*/\1/')
                        hits=$(echo "$pct $total" | awk '{printf "%d", $1*$2/100}')
                        echo "$current_file $total $hits" >> "$RAW"
                    fi
                    ;;
            esac
        done

        rm -f ./*.gcov
    fi

    echo -n "." >&2
done

echo "" >&2

if [ ! -s "$RAW" ]; then
    echo "No coverage data collected." >&2
    exit 1
fi

sort "$RAW" | awk '
{
    file = $1; total = $2; hits = $3
    if (file != prev && prev != "") {
        printf "%-45s %4d / %4d  (%5.1f%%)\n", prev, mh, mt, (mt > 0 ? mh * 100.0 / mt : 0)
        gh += mh; gt += mt
    }
    if (file != prev) { mh = hits; mt = total }
    else { if (hits > mh) mh = hits; if (total > mt) mt = total }
    prev = file
}
END {
    if (prev != "") {
        printf "%-45s %4d / %4d  (%5.1f%%)\n", prev, mh, mt, (mt > 0 ? mh * 100.0 / mt : 0)
        gh += mh; gt += mt
    }
    printf "\n%-45s %4d / %4d  (%5.1f%%)\n", "TOTAL (max per file, conservative)", gh, gt, (gt > 0 ? gh * 100.0 / gt : 0)
}
'

rm -rf out/
