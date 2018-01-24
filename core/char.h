bool Char__EQ_(char a, char b) {
  return a == b;
}

string Char_str(char c) {
    string buffer = CARP_MALLOC(3);
    snprintf(buffer, 3, "\\%c", c);
    return buffer;
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

string Char_format(string* str, char b) {
    int size = snprintf(NULL, 0, *str, b)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, b);
    return buffer;
}


