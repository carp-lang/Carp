#include <stdbool.h>

bool Char__EQ_(char a, char b) {
  return a == b;
}

bool Char__LT_(char a, char b) {
  return a < b;
}

bool Char__GT_(char a, char b) {
  return a > b;
}

int Char_to_MINUS_int(char c) {
  return (int)(unsigned char)c;
}

char Char_from_MINUS_int(int i) {
  return (char)i;
}

char Char_copy(char *c) {
  return *c;
}

String PtrChar_str(char *c) {
    size_t len = strlen(c) + 1;
    String ptr = CARP_MALLOC(len);

    if (ptr == NULL) {
      return NULL;
    }

    return (String) memcpy(ptr, c, len);
}
