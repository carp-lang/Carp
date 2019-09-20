if [ -z "$CARP" ]
then
    if [ -z "$NIX_CC" ]
    then
	CARP="stack exec carp --"
    else
	CARP="cabal -v0 new-run carp --"
    fi
fi
$CARP $*
