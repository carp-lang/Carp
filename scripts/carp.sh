#!/usr/bin/env sh
if [ -z "$CARP" ]
then
    if [ -z "$NIX_CC" ]
    then
	CARP="stack exec carp"
    else
	CARP="cabal -v0 run carp"
    fi
    CARP="$CARP $BUILD_OPTS --"
fi
export CARP_DIR=`pwd`
# TODO: Temporary band-aid to make different versions of clang run tests OK when supported flag sets differ
EVAL='(Project.config "cflag" (cons "-Wno-unknown-warning-option" (Project.get-config "cflag")))'
$CARP --eval-preload "\'$EVAL\'" $CARP_OPTS "$@"
