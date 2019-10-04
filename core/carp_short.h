typedef short Short;
#define T Short
#include "carp_integral.h"
#undef T

String Short_str(Short x) {
  String Int_str(int);
  return Int_str(x);
}
