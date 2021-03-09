#!/usr/bin/env sh

set -e; # will make the script stop if there are any errors
set -u; # will make the script stop if there is use of undefined

NO_SDL=0
if [ $# -gt 0 ] && [ "$1" = "--no_sdl" ]; then
    NO_SDL=1
fi

./scripts/build.sh

echo "Build and run some examples"
./test/execute.sh ./examples/functor.carp
./test/execute.sh ./examples/external_struct.carp
./test/execute.sh ./examples/updating.carp
./test/execute.sh ./examples/sorting.carp
./test/execute.sh ./examples/generic_structs.carp
./test/execute.sh ./examples/maps.carp
./test/execute.sh ./examples/sumtypes.carp

echo "Build and run some tests that print (check their output)"
./test/execute.sh ./test/produces-output/basics.carp
./test/execute.sh ./test/produces-output/function_members.carp
./test/execute.sh ./test/produces-output/globals.carp
./test/execute.sh ./test/produces-output/lambdas.carp
./test/execute.sh ./test/produces-output/setting_variables.carp

echo "Actual tests (using the test suite)"
for f in ./test/*.carp; do
    echo $f
   ./scripts/carp.sh -x --log-memory $f
    echo
done

echo "Test for correct error messages when doing "carp --check" on the source."
for f in ./test/test-for-errors/*.carp; do
    echo $f
   ./test/check.sh $f
done

echo "Make sure the benchmarks compile."
for f in ./bench/*.carp; do
    echo $f
    ./scripts/carp.sh -b $f
done

echo "Compile-only examples"
compileOnlyExamples="\
                   examples/mutual_recursion.carp \
                   examples/guessing_game.carp \
                   examples/unicode.carp \
                   examples/benchmark_*.carp \
                   examples/nested_lambdas.carp
"

for e in $compileOnlyExamples ; do
    echo $e
    ./scripts/carp.sh $e -b
done

# Make sure a no-core build works
echo ./examples/no_core.carp
./scripts/carp.sh ./examples/no_core.carp --no-core --no-profile -b

# Run tests which rely on SDL unless the `--no_sdl` argument was passed in
if [ ${NO_SDL} -eq 0 ]; then
    echo "Compile-only SDL examples"
    compileOnlySdlExamples="
        examples/langtons_ant.carp
        examples/reptile.carp
        examples/carp_demo.carp
        examples/minimal_sdl.carp
        examples/sounds.carp
        examples/fonts.carp
    "

    for e in $compileOnlySdlExamples ; do
        echo $e
        ./scripts/carp.sh $e -b
    done
fi

# Generate docs
./scripts/carp.sh ./docs/core/generate_core_docs.carp

# Generate SDL docs unless the `--no_sdl` argument was passed in
if [ ${NO_SDL} -eq 0 ]; then
  ./scripts/carp.sh ./docs/sdl/generate_sdl_docs.carp
fi

echo "ALL TESTS DONE."
