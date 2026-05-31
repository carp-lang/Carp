#!/usr/bin/env sh

set -e; # will make the script stop if there are any errors
set -u; # will make the script stop if there is use of undefined

NO_SDL=0
JOBS=1

usage() {
    cat <<'EOF' >&2
Usage: run_carp_tests.sh [-j N] [--no_sdl]
  -j N      Run independent test files with N parallel workers (default: 1).
            Parallel mode also bypasses `stack exec` and isolates each job's
            output directory under out/par/ to avoid clobbering.
  --no_sdl  Skip SDL-dependent tests.
EOF
}

while [ $# -gt 0 ]; do
    case "$1" in
        --no_sdl) NO_SDL=1 ;;
        -j)
            shift
            if [ $# -eq 0 ]; then usage; exit 1; fi
            JOBS=$1
            ;;
        -j*) JOBS=${1#-j} ;;
        -h|--help) usage; exit 0 ;;
        *)
            echo "Unknown argument: $1" >&2
            usage
            exit 1
            ;;
    esac
    shift
done

case "$JOBS" in
    ''|*[!0-9]*)
        echo "-j expects a positive integer, got: $JOBS" >&2
        exit 1
        ;;
esac
[ "$JOBS" -lt 1 ] && JOBS=1

./scripts/build.sh

# In parallel mode, resolve the built binary directly to skip ~170ms of
# `stack exec` startup per invocation, and tell helper scripts to use
# isolated output directories so concurrent jobs don't clobber ./out/Untitled.
if [ "$JOBS" -gt 1 ]; then
    if [ -z "${CARP:-}" ] && [ -z "${NIX_CC:-}" ]; then
        CARP_INSTALL_ROOT=$(stack path --local-install-root 2>/dev/null || true)
        if [ -n "$CARP_INSTALL_ROOT" ] && [ -x "$CARP_INSTALL_ROOT/bin/carp" ]; then
            export CARP="$CARP_INSTALL_ROOT/bin/carp"
        fi
    fi
    export CARP_PARALLEL=1
    mkdir -p out/par
fi

# Shell snippet run by each parallel worker for carp.sh-based loops.
# Positional args: $1 = .carp file, $2 = flag string (word-split intentionally).
# In parallel mode an isolated output directory is forced via --eval-preload
# and cleaned up on exit; in sequential mode this is a direct carp.sh call.
WORKER='
    set -e
    echo "$1"
    if [ "${CARP_PARALLEL:-0}" = "1" ]; then
        OUT="out/par/$$-$(basename "$1" .carp)"
        mkdir -p "$OUT"
        trap '\''rm -rf "$OUT"'\'' EXIT
        ./scripts/carp.sh --eval-preload "(Project.config \"output-directory\" \"$OUT\")" $2 "$1"
    else
        ./scripts/carp.sh $2 "$1"
    fi
'

echo "Build and run some examples"
printf '%s\n' \
    ./examples/functor.carp \
    ./examples/external_struct.carp \
    ./examples/updating.carp \
    ./examples/sorting.carp \
    ./examples/generic_structs.carp \
    ./examples/maps.carp \
    ./examples/sumtypes.carp \
    ./examples/json_parser.carp \
    | xargs -n1 -P"$JOBS" -I{} sh -c './test/execute.sh "$1"' _ {}

echo "Build and run some tests that print (check their output)"
printf '%s\n' \
    ./test/produces-output/basics.carp \
    ./test/produces-output/function_members.carp \
    ./test/produces-output/globals.carp \
    ./test/produces-output/lambdas.carp \
    ./test/produces-output/recursive_types.carp \
    ./test/produces-output/recursive_type_decl_only.carp \
    ./test/produces-output/maybe_custom_member_decl_only.carp \
    ./test/produces-output/setting_variables.carp \
    ./test/produces-output/set_ref_valid.carp \
    ./test/produces-output/forward_references.carp \
    ./test/produces-output/explicit_lifetimes.carp \
    ./test/produces-output/repl.carp \
    | xargs -n1 -P"$JOBS" -I{} sh -c './test/execute.sh "$1"' _ {}

echo "Actual tests (using the test suite)"
printf '%s\n' ./test/*.carp \
    | xargs -n1 -P"$JOBS" -I{} sh -c "$WORKER" _ {} '-x --log-memory'

echo "Test for correct error messages when doing \"carp --check\" on the source."
printf '%s\n' ./test/test-for-errors/*.carp \
    | xargs -n1 -P"$JOBS" -I{} sh -c 'set -e; echo "$1"; ./test/check.sh "$1"' _ {}

echo "Make sure the benchmarks compile."
printf '%s\n' ./bench/*.carp \
    | xargs -n1 -P"$JOBS" -I{} sh -c "$WORKER" _ {} '-b'

echo "Compile-only examples"
printf '%s\n' \
    examples/mutual_recursion.carp \
    examples/guessing_game.carp \
    examples/unicode.carp \
    examples/benchmark_*.carp \
    examples/nested_lambdas.carp \
    | xargs -n1 -P"$JOBS" -I{} sh -c "$WORKER" _ {} '-b'

# Make sure a no-core build works
echo ./examples/no_core.carp
./scripts/carp.sh ./examples/no_core.carp --no-core --no-profile -b

# Run tests which rely on SDL unless the `--no_sdl` argument was passed in
if [ ${NO_SDL} -eq 0 ]; then
    echo "Compile-only SDL examples"
    printf '%s\n' \
        examples/langtons_ant.carp \
        examples/reptile.carp \
        examples/carp_demo.carp \
        examples/minimal_sdl.carp \
        examples/sounds.carp \
        examples/fonts.carp \
        | xargs -n1 -P"$JOBS" -I{} sh -c "$WORKER" _ {} '-b'
fi

# Generate docs
./scripts/carp.sh ./docs/core/generate_core_docs.carp

# Generate SDL docs unless the `--no_sdl` argument was passed in
if [ ${NO_SDL} -eq 0 ]; then
  ./scripts/carp.sh ./docs/sdl/generate_sdl_docs.carp
fi

# Clean up any stray isolated output directories.
if [ "$JOBS" -gt 1 ] && [ -d out/par ]; then
    rm -rf out/par
fi

echo "ALL TESTS DONE."
