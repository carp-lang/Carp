#!/usr/bin/env bash
if [ -z "$CARP" ]
then
    if [ -z "$NIX_CC" ]
    then
	stack build
    else
	cabal build
    fi
fi
command -v clang-format >/dev/null && clang-format -i core/*.h
