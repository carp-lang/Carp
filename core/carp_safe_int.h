#ifndef _WIN32
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
