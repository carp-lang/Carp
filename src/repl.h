#pragma once

#include "obj.h"
#include <setjmp.h>

void repl(Process *process);

jmp_buf jumpbuffer;
