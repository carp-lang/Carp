#include <math.h>
#include "carp_stdbool.h"

Long Long__PLUS_(Long x, Long y)   { return x + y; }
Long Long__MINUS_(Long x, Long y)  { return x - y; }
Long Long__MUL_(Long x, Long y)    { return x * y; }
Long Long__DIV_(Long x, Long y)    { return x / y; }
#ifndef _WIN32
bool Long_safe_MINUS_add(Long x, Long y, Long* res) { return __builtin_saddll_overflow(x, y, res); }
bool Long_safe_MINUS_sub(Long x, Long y, Long* res) { return __builtin_ssubll_overflow(x, y, res); }
bool Long_safe_MINUS_mul(Long x, Long y, Long* res) { return __builtin_smulll_overflow(x, y, res); }
#endif
bool Long__EQ_(Long x, Long y)     { return x == y; }
bool Long__LT_(Long x, Long y)    { return x < y; }
bool Long__GT_(Long x, Long y)    { return x > y; }
Long Long_neg(Long x) { return -x; }

Long Long_inc(Long x) { return x + 1; }
Long Long_dec(Long x) { return x - 1; }
Long Long_abs(Long x) { return x > 0 ? x : -x; }
Long Long_bit_MINUS_shift_MINUS_left(Long x, Long y) { return x << y; }
Long Long_bit_MINUS_shift_MINUS_right(Long x, Long y) { return x >> y; }
Long Long_bit_MINUS_and(Long x, Long y) { return x & y; }
Long Long_bit_MINUS_or(Long x, Long y) { return x | y; }
Long Long_bit_MINUS_xor(Long x, Long y) { return x ^ y; }
Long Long_bit_MINUS_not(Long x) { return ~x; }

Long Long_copy(const Long *x) { return *x; }

Long Long_mod(Long x, Long divider) {
    return x % divider;
}

void Long_seed(Long seed) {
    srand(seed);
}

bool Long_mask(Long a, Long b) {
    return a & b;
}

int Long_to_MINUS_int(Long a) {
  return (int) a;
}

Long Long_from_MINUS_int(int a) {
  return (Long) a;
}
