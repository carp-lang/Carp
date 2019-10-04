
typedef char Char;
#define T Char
#include "carp_integral.h"
#undef T

String PtrChar_str(const char *c) {
    size_t len = strlen(c) + 1;
    String ptr = CARP_MALLOC(len);
    return (String)memcpy(ptr, c, len);
}
