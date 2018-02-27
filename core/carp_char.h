#include <stdbool.h>

bool Char__EQ_(char a, char b) {
  return a == b;
}

int Char_to_MINUS_int(char c) {
  return (int)c;
}

char Char_from_MINUS_int(int i) {
  return (char)i;
}

char Char_copy(char *c) {
  return *c;
}
