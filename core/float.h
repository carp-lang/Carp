float Float__PLUS_(float x, float y) { return x + y; }
float Float__MINUS_(float x, float y) { return x - y; }
float Float__MUL_(float x, float y) { return x * y; }
float Float__DIV_(float x, float y) { return x / y; }
bool Float__LT_(float x, float y) { return x < y; }
bool Float__GT_(float x, float y) { return x > y; }
bool Float__EQ_(float x, float y) { return x == y; }
double Float_neg(float x) { return -x; }

float Float_copy(float *x) { return *x; }

int Float_to_MINUS_int(float x) {
    return (int)x;
}

float Float_from_MINUS_int(int x) {
    return (float)x;
}

float Float_random() {
    return rand();
}

float Float_random_MINUS_between(float lower, float upper) {
    float diff = upper - lower;
    float r = ((float)(rand() % INT_MAX)) / ((float)INT_MAX);
    return lower + diff * r;
}

float Float_abs(float x) {
    return fabs(x);
}

float Float_acos(float x) {
    return acos(x);
}

float Float_asin(float x) {
    return asin(x);
}

float Float_atan(float x) {
    return atan(x);
}

float Float_atan2(float y, float x) {
    return atan2(y, x);
}

float Float_cos(float x) {
    return cos(x);
}

float Float_cosh(float x) {
    return cosh(x);
}

float Float_sin(float x) {
    return sin(x);
}

float Float_sinh(float x) {
    return sinh(x);
}

float Float_tanh(float x) {
    return tanh(x);
}

float Float_exp(float x) {
    return exp(x);
}

float Float_frexp(float x, int* exponent) {
    return frexp(x, exponent);
}

float Float_ldexp(float x, int exponent) {
    return ldexp(x, exponent);
}

float Float_log(float x) {
    return log(x);
}

float Float_log10(float x) {
    return log10(x);
}

float Float_modf(float x, float* integer) {
    return modf(x, (double*) integer);
}

float Float_pow(float x, float y) {
    return pow(x, y);
}

float Float_sqrt(float x) {
    return sqrt(x);
}

float Float_ceil(float x) {
    return ceil(x);
}

float Float_floor(float x) {
    return floor(x);
}

float Float_mod(float x, float y) {
    return fmod(x, y);
}

string Float_str(float x) {
    int size = snprintf(NULL, 0, "%gf", x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, "%gf", x);
    return buffer;
}

string Float_format(string* str, float x) {
    int size = snprintf(NULL, 0, *str, x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, x);
    return buffer;
}

