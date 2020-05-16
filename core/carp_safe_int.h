#if defined _WIN32 || defined __TINYC__

bool Int_safe_MINUS_add(int x, int y, int* res) {
    int r = x + y;
    *res = r;
    return (y > 0) && (x > (INT_MAX - y)) || (y < 0) && (x < (INT_MIN - y));
}
bool Int_safe_MINUS_sub(int x, int y, int* res) {
    int r = x - y;
    *res = r;
    *res = x - y;
    return (y > 0 && x < (INT_MIN + y)) || (y < 0 && x > (INT_MAX + y));
}
bool Int_safe_MINUS_mul(int x, int y, int* res) {
    int r = x * y;
    *res = r;
    return y == 0 || (r / y) != x;
}
#else
bool Int_safe_MINUS_add(int x, int y, int* res) {
    return __builtin_add_overflow(x, y, res);
}
bool Int_safe_MINUS_sub(int x, int y, int* res) {
    return __builtin_sub_overflow(x, y, res);
}
bool Int_safe_MINUS_mul(int x, int y, int* res) {
    return __builtin_mul_overflow(x, y, res);
}
#endif
