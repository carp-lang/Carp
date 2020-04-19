if [ -z "$CARP" ]
then
    if [ -z "$NIX_CC" ]
    then
	stack build
    else
	cabal build
    fi
fi
if which clang-format > /dev/null
then
  clang-format -i core/*.h
fi
