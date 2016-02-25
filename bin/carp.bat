@ECHO OFF

REM Get the dir of this script (which is in the bin folder)
SET DIR=%~dp0

SET WRAPPER=
REM WRAPPER=rlwrap
REM WRAPPER=lldb
REM WRAPPER=valgrind

SET CARP_DIR=%DIR%..\

%WRAPPER% "%DIR%carp-repl"
