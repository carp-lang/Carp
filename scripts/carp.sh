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
$CARP $CARP_OPTS "$@"
