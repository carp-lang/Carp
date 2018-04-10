#!/bin/sh

carp $1 --log-memory -x > test/output/$1.output.expected 2>&1
