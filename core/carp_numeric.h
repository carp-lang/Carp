T NAME(_MUL_)(T x, T y)    { return x * y; }
T NAME(_PLUS_)(T x, T y)   { return x + y; }
T NAME(_MINUS_)(T x, T y)  { return x - y; }
T NAME(_DIV_)(T x, T y)    { return x / y; }
bool NAME(_LT_)(T x, T y)    { return x < y; }
bool NAME(_EQ_)(T x, T y)    { return x == y; }
bool NAME(_GT_)(T x, T y)    { return x > y; }
T NAME(copy)(const T *x) { return *x; }
T NAME(dec)(T x) { return x - 1; }
T NAME(from_MINUS_floating)(Floating a) { return (T)a; }
T NAME(from_MINUS_integral)(Integral a) { return (T)a; }
T NAME(inc)(T x) { return x + 1; }
T NAME(neg)(T x)    { return -x; }
Integral NAME(to_MINUS_bytes)(T x) {
  Integral y = 0;
  __builtin_memcpy(&y, &x, sizeof(x)); // FIXME will fail on big-endian
  return y;
}
Floating NAME(to_MINUS_floating)(T a) { return (Floating)a; }
Integral NAME(to_MINUS_integral)(a) { return (Integral)a; }
