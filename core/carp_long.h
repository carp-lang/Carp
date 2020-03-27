long Long__PLUS_(long x, long y) {
    return x + y;
}
long Long__MINUS_(long x, long y) {
    return x - y;
}
long Long__MUL_(long x, long y) {
    return x * y;
}
long Long__DIV_(long x, long y) {
    return x / y;
}
#ifndef _WIN32
bool Long_safe_MINUS_add(long x, long y, long* res) {
    return __builtin_saddl_overflow(x, y, res);
}
bool Long_safe_MINUS_sub(long x, long y, long* res) {
    return __builtin_ssubl_overflow(x, y, res);
}
bool Long_safe_MINUS_mul(long x, long y, long* res) {
    return __builtin_smull_overflow(x, y, res);
}
#endif
bool Long__EQ_(long x, long y) {
    return x == y;
}
bool Long__LT_(long x, long y) {
    return x < y;
}
bool Long__GT_(long x, long y) {
    return x > y;
}
long Long_neg(long x) {
    return -x;
}

long Long_inc(long x) {
    return x + 1;
}
long Long_dec(long x) {
    return x - 1;
}
long Long_abs(long x) {
    return x > 0 ? x : -x;
}
long Long_bit_MINUS_shift_MINUS_left(long x, long y) {
    return x << y;
}
long Long_bit_MINUS_shift_MINUS_right(long x, long y) {
    return x >> y;
}
long Long_bit_MINUS_and(long x, long y) {
    return x & y;
}
long Long_bit_MINUS_or(long x, long y) {
    return x | y;
}
long Long_bit_MINUS_xor(long x, long y) {
    return x ^ y;
}
long Long_bit_MINUS_not(long x) {
    return ~x;
}

long Long_copy(const long* x) {
    return *x;
}

long Long_mod(long x, long divider) {
    return x % divider;
}

void Long_seed(long seed) {
    srand(seed);
}

bool Long_mask(long a, long b) {
    return a & b;
}

int Long_to_MINUS_int(long a) {
    return (int)a;
}

long Long_from_MINUS_int(int a) {
    return (long)a;
}
