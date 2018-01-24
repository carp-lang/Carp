int Int__PLUS_(int x, int y)   { return x + y; }
int Int__MINUS_(int x, int y)  { return x - y; }
int Int__MUL_(int x, int y)    { return x * y; }
int Int__DIV_(int x, int y)    { return x / y; }
bool Int_safe_MINUS_add(int x, int y, int* res) { return __builtin_sadd_overflow(x, y, res); }
bool Int_safe_MINUS_sub(int x, int y, int* res) { return __builtin_ssub_overflow(x, y, res); }
bool Int_safe_MINUS_mul(int x, int y, int* res) { return __builtin_smul_overflow(x, y, res); }
bool Int__EQ_(int x, int y)     { return x == y; }
bool Int__DIV__EQ_(int x, int y) { return x != y; }
bool Int__LT_(int x, int y)    { return x < y; }
bool Int__GT_(int x, int y)    { return x > y; }

int Int_inc(int x) { return x + 1; }
int Int_dec(int x) { return x - 1; }
int Int_abs(int x) { return abs(x); }
int Int_bit_MINUS_shift_MINUS_left(int x, int y) { return x << y; }
int Int_bit_MINUS_shift_MINUS_right(int x, int y) { return x >> y; }
int Int_bit_MINUS_and(int x, int y) { return x & y; }
int Int_bit_MINUS_or(int x, int y) { return x | y; }
int Int_bit_MINUS_xor(int x, int y) { return x ^ y; }
int Int_bit_MINUS_not(int x) { return ~x; }

int Int_copy(int *x) { return *x; }

int Int_from_MINUS_string(string *s) {
    return atoi(*s);
}

int Int_mod(int x, int divider) {
    return x % divider;
}

void Int_seed(int seed) {
    srand(seed);
}

int Int_random() {
    return rand();
}

int Int_random_MINUS_between(int lower, int upper) {
    int diff = upper - lower;
    return lower + (rand() % diff);
}

string Int_str(int x) {
    int size = snprintf(NULL, 0, "%d", x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, "%d", x);
    return buffer;
}

string Int_format(string* str, int x) {
    int size = snprintf(NULL, 0, *str, x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, x);
    return buffer;
}

bool Int_mask(int a, int b) {
    return a & b;
}

