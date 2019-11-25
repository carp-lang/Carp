bool Int_safe_MINUS_add(int x, int y, int* res) {
    return __builtin_sadd_overflow(x, y, res);
}
bool Int_safe_MINUS_sub(int x, int y, int* res) {
    return __builtin_ssub_overflow(x, y, res);
}
bool Int_safe_MINUS_mul(int x, int y, int* res) {
    return __builtin_smul_overflow(x, y, res);
}
