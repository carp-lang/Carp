#define NAME(x) CAT(T, x)

#include "carp_numeric.h"

T NAME(abs)(T x) { return SUF(fabs)(x); }
T NAME(acos)(T x) { return SUF(acos)(x); }
T NAME(asin)(T x) { return SUF(asin)(x); }
T NAME(atan)(T x) { return SUF(atan)(x); }
T NAME(atan2)(T y, T x) { return SUF(atan2)(y, x); }
T NAME(cos)(T x) { return SUF(cos)(x); }
T NAME(cosh)(T x) { return SUF(cosh)(x); }
T NAME(sin)(T x) { return SUF(sin)(x); }
T NAME(sinh)(T x) { return SUF(sinh)(x); }
T NAME(tan)(T x) {  return SUF(tan)(x); }
T NAME(tanh)(T x) { return SUF(tanh)(x); }
T NAME(exp)(T x) { return SUF(exp)(x); }
T NAME(frexp)(T x, int* exponent) { return SUF(frexp)(x, exponent); }
T NAME(ldexp)(T x, int exponent) { return SUF(ldexp)(x, exponent); }
T NAME(log)(T x) { return SUF(log)(x); }
T NAME(log10)(T x) { return SUF(log10)(x); }
T NAME(modf)(T x, T * integer) { return SUF(modf)(x, integer); }
T NAME(pow)(T x, T y) { return SUF(pow)(x, y); }
T NAME(sqrt)(T x) { return SUF(sqrt)(x); }
T NAME(ceil)(T x) { return SUF(ceil)(x); }
T NAME(floor)(T x) { return SUF(floor)(x); }
T NAME(mod)(T x, T y) { return SUF(fmod)(x, y); }

#undef NAME
