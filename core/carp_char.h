bool Char__EQ_(Char a, Char b) {
    return a == b;
}

bool Char__LT_(Char a, Char b) {
    return a < b;
}

bool Char__GT_(Char a, Char b) {
    return a > b;
}

int Char_to_MINUS_int(Char c) {
    return (int)c;
}

Char Char_from_MINUS_int(int i) {
    return (Char)i;
}

int Char_to_MINUS_byte(Char c) {
    return (uint8_t)c;
}

Char Char_from_MINUS_byte(uint8_t i) {
    return (Char)i;
}

Char Char_copy(const Char *c) {
    return *c;
}

String PtrChar_str(const char *c) {
    size_t len = strlen(c) + 1;
    String ptr = CARP_MALLOC(len);
    return (String)memcpy(ptr, c, len);
}
