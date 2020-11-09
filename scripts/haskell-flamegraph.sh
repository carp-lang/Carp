#! /usr/bin/env bash
# Usage:
#
# ./haskell-flamegraph <carp options>
BUILD_OPTS="--enable-profiling --enable-library-profiling" ./scripts/carp.sh $* +RTS -p -hc
ghc-prof-flamegraph carp.prof
xdg-open carp.svg
