
typedef float Float;
#define T Float
#define SUF(x) x##f
#include "carp_floating.h"
#undef SUF
#undef T

const Float Float_MAX = FLT_MAX;
