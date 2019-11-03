#include <fenv.h>

static void enable_exceptions(int e) {
    fenv_t fe;
    fegetenv(&fe);
#if !defined __linux__
    fe.__control &= ~e;
#endif
    fe.__mxcsr &= ~(e << 7);
    fesetenv(&fe);
}
