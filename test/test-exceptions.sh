#!/bin/sh
RES=$1
run() {
    trap echo SIGFPE
    out/Untitled
}
./carp.sh -b $1
run >test/output/$1.output.actual 2>&1
if ! diff test/output/$1.output.actual test/output/$1.`uname -s`.output.expected; then
    echo "$1 failed."
    exit 1
else
    rm test/output/$1.output.actual
    exit 0
fi
