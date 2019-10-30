const double CARP_DBL_MAX = DBL_MAX;

double Double__PLUS_(double x, double y) {
    return x + y;
}
double Double__MINUS_(double x, double y) {
    return x - y;
}
double Double__MUL_(double x, double y) {
    return x * y;
}
double Double__DIV_(double x, double y) {
    return x / y;
}
bool Double__LT_(double x, double y) {
    return x < y;
}
bool Double__GT_(double x, double y) {
    return x > y;
}
bool Double__EQ_(double x, double y) {
    return x == y;
}
double Double_neg(double x) {
    return -x;
}

double Double_copy(const double* x) {
    return *x;
}

// Double.toInt : Double -> Int
int Double_to_MINUS_int(double x) {
    return (int)x;
}

double Double_from_MINUS_int(int x) {
    return (double)x;
}

long Double_to_MINUS_bytes(double x) {
    long y;
    memcpy(&y, &x, sizeof(double));
    return y;
}

float Double_to_MINUS_float(double x) {
    return (float)x;
}

double Double_from_MINUS_float(float x) {
    return (double)x;
}

long Double_to_MINUS_long(double x) {
    return (long)x;
}

double Double_from_MINUS_long(long x) {
    return (double)x;
}

double Double_abs(double x) {
    return x > 0.0 ? x : -x;
}

double Double_acos(double x) {
    return acos(x);
}

double Double_asin(double x) {
    return asin(x);
}

double Double_atan(double x) {
    return atan(x);
}

double Double_atan2(double y, double x) {
    return atan2(y, x);
}

double Double_cos(double x) {
    return cos(x);
}

double Double_cosh(double x) {
    return cosh(x);
}

double Double_sin(double x) {
    return sin(x);
}

double Double_sinh(double x) {
    return sinh(x);
}

double Double_tanh(double x) {
    return tanh(x);
}

double Double_exp(double x) {
    return exp(x);
}

double Double_frexp(double x, int* exponent) {
    return frexp(x, exponent);
}

double Double_ldexp(double x, int exponent) {
    return ldexp(x, exponent);
}

double Double_log(double x) {
    return log(x);
}

double Double_log10(double x) {
    return log10(x);
}

double Double_modf(double x, double* integer) {
    return modf(x, integer);
}

double Double_pow(double x, double y) {
    return pow(x, y);
}

double Double_sqrt(double x) {
    return sqrt(x);
}

double Double_ceil(double x) {
    return ceil(x);
}

double Double_floor(double x) {
    return floor(x);
}

double Double_mod(double x, double y) {
    return fmod(x, y);
}
