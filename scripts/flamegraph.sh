#!/usr/bin/env sh
# To enable the required permissions, execute the following sequence as root:
# $ chown root:wheel /proc/modules
# $ chown root:wheel /proc/kallsyms
# $ echo 0 > /proc/sys/kernel/kptr_restrict
# $ echo -1 > /proc/sys/kernel/perf_event_paranoid
#
# Usage:
#
# Profile an executable
# ./flamegraph.sh <executable>
#
# To change the frequency:
# HZ=100 ./flamegraph.sh <executable>
#
# To use the maximum frequency
# HZ=max ./flamegraph.sh <executable>
#
# This can be useful to see how much time is spent in carp vs clang
# ./flamegraph.sh ./carp.sh -b <file.carp>

HZ=${HZ:-1000}
prg=`basename $1`
svg=flamegraph.svg
perf record -F $HZ -a -g -- $*
perf script | stackcollapse-perf.pl | flamegraph.pl > $svg
xdg-open $svg
