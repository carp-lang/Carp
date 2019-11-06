#define NAME(x) CAT(T, x)

#include "carp_numeric.h"

const T NAME(MIN) = (T)(1ULL << (8 * sizeof(T) - 1));
const T NAME(MAX) = ~(NAME(MIN));

T NAME(abs)(T x) {
    return x >= 0 ? x : -x;
}
T NAME(bit_MINUS_and)(T x, T y) {
    return x & y;
}
T NAME(bit_MINUS_not)(T x) {
    return ~x;
}
T NAME(bit_MINUS_or)(T x, T y) {
    return x | y;
}
T NAME(bit_MINUS_shift_MINUS_left)(T x, T y) {
    return x << y;
}
T NAME(bit_MINUS_shift_MINUS_right)(T x, T y) {
    return x >> y;
}
T NAME(bit_MINUS_xor)(T x, T y) {
    return x ^ y;
}
T NAME(mod)(T x, T divider) {
    return x % divider;
}

#undef NAME
