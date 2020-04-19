const float CARP_FLT_MAX = FLT_MAX;

float Float__PLUS_(float x, float y) {
    return x + y;
}
float Float__MINUS_(float x, float y) {
    return x - y;
}
float Float__MUL_(float x, float y) {
    return x * y;
}
float Float__DIV_(float x, float y) {
    return x / y;
}
bool Float__LT_(float x, float y) {
    return x < y;
}
bool Float__GT_(float x, float y) {
    return x > y;
}
bool Float__EQ_(float x, float y) {
    return x == y;
}
float Float_neg(float x) {
    return -x;
}

float Float_copy(const float* x) {
    return *x;
}

int Float_to_MINUS_int(float x) {
    return (int)x;
}

float Float_from_MINUS_int(int x) {
    return (float)x;
}

int Float_to_MINUS_bytes(float x) {
    int y;
    memcpy(&y, &x, sizeof(float));
    return y;
}

float Float_abs(float x) {
    return fabsf(x);
}

float Float_acos(float x) {
    return acosf(x);
}

float Float_asin(float x) {
    return asinf(x);
}

float Float_atan(float x) {
    return atanf(x);
}

float Float_atan2(float y, float x) {
    return atan2f(y, x);
}

float Float_cos(float x) {
    return cosf(x);
}

float Float_cosh(float x) {
    return coshf(x);
}

float Float_sin(float x) {
    return sinf(x);
}

float Float_sinh(float x) {
    return sinhf(x);
}

float Float_tan(float x) {
    return tanf(x);
}

float Float_tanh(float x) {
    return tanhf(x);
}

float Float_exp(float x) {
    return expf(x);
}

float Float_frexp(float x, int* exponent) {
    return frexpf(x, exponent);
}

float Float_ldexp(float x, int exponent) {
    return ldexpf(x, exponent);
}

float Float_log(float x) {
    return logf(x);
}

float Float_log10(float x) {
    return log10f(x);
}

float Float_modf(float x, float* integer) {
    return modff(x, integer);
}

float Float_pow(float x, float y) {
    return powf(x, y);
}

float Float_sqrt(float x) {
    return sqrtf(x);
}

float Float_ceil(float x) {
    return ceilf(x);
}

float Float_floor(float x) {
    return floorf(x);
}

float Float_mod(float x, float y) {
    return fmodf(x, y);
}
