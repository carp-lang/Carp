#pragma once

#include "obj.h"
#include <setjmp.h>

void repl(Process *process);

extern jmp_buf jumpbuffer;
