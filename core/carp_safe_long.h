bool Long_safe_MINUS_add(long x, long y, long* res) { return __builtin_saddl_overflow(x, y, res); }
bool Long_safe_MINUS_sub(long x, long y, long* res) { return __builtin_ssubl_overflow(x, y, res); }
bool Long_safe_MINUS_mul(long x, long y, long* res) { return __builtin_smull_overflow(x, y, res); }
