if [ -z "$CARP" ]
then
    if [ -z "$NIX_CC" ]
    then
	stack build
    else
	cabal build
    fi
fi
