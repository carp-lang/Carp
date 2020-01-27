String String_allocate(int len, char byte) {
    /* Allocate a string of length 'len + 1'
     * setting the first len bytes to byte
     * and adding a null terminator
     *
     * String_alloc(10, "a") == "aaaaaaaaaa"
     */
    String ptr = CARP_MALLOC(len + 1);
    memset(ptr, byte, len);
    ptr[len] = '\0';
    return ptr;
}

void String_delete(String s) {
    CARP_FREE(s);
}

void String_string_MINUS_set_BANG_(String *s, int i, char ch) {
    CHK_INDEX(i, strlen(*s));
    (*s)[i] = ch;
}

void String_string_MINUS_set_MINUS_at_BANG_(String *into, int i,
                                            const String *src) {
    char *dest = (*into) + i;
    int lsrc = strlen(*src);
    /* given a string and indices
     *
     *  0 1 2 3 4 5 6 7 8 9
     * "a b c d e f g h i j"
     * linto = strlen(...) = 10
     *
     * if we want to insert at '6' a string of length '4'
     *
     *  0 1 2 3
     * "w x y z"
     * lsrc = strlen(...) = 4
     *
     * we need to make sure that the new string will not grow the first
     *
     *  0 1 2 3 4 5 6 7 8 9
     * "a b c d e f g h i j"
     *              ^
     *              |
     *              0 1 2 3
     *             "w x y z"
     *
     * we check this by
     *      (i + lsrc) < (linto + 1)
     *      (6 +    4) < (10    + 1)
     *      10         < 11
     *      true
     *
     * so this write is safe
     */
    CHK_INDEX(i + lsrc, strlen(*into) + 1);
    memcpy(dest, *src, lsrc);
}

String String_copy(const String *s) {
    size_t len = strlen(*s) + 1;
    String ptr = CARP_MALLOC(len);
    return (String)memcpy(ptr, *s, len);
}

bool String__EQ_(const String *a, const String *b) {
    return strcmp(*a, *b) == 0;
}

bool String__GT_(const String *a, const String *b) {
    return strcmp(*a, *b) > 0;
}

bool String__LT_(const String *a, const String *b) {
    return strcmp(*a, *b) < 0;
}

String String_append(const String *a, const String *b) {
    String buffer;
    asprintf(&buffer, "%s%s", *a, *b);
    return buffer;
}

int String_length(const String *s) {
    return strlen(*s);
}

char *String_cstr(const String *s) {
    return *s;
}

String String_from_MINUS_cstr(char *s) {
    return String_copy(&s);
}

String String_str(const String *s) {
    return String_copy(s);
}

String String_prn(const String *s) {
    String buffer;
    asprintf(&buffer, "@\"%s\"", *s);
    return buffer;
}

char String_char_MINUS_at(const String *s, int i) {
    return (*s)[i];
}

String String_format(const String *str, const String *s) {
    String buffer;
    asprintf(&buffer, *str, *s);
    return buffer;
}

Array String_chars(const String *s) {
    Array chars;
    chars.len = strlen(*s);
    chars.capacity = chars.len;
    chars.data = String_copy(s);
    return chars;
}

String String_from_MINUS_chars(const Array *a) {
    String s = CARP_MALLOC(a->len + 1);
    memcpy(s, a->data, a->len);
    s[a->len] = '\0';
    return s;
}

String String_tail(const String *s) {
    int len = strlen(*s);
    String news = CARP_MALLOC(len);
    memcpy(news, (*s) + 1, len - 1);
    news[len - 1] = '\0';
    return news;
}

String String_empty() {
    String s = CARP_MALLOC(1);
    s[0] = '\0';
    return s;
}

String Bool_str(bool b) {
    const String true_str = "true";
    const String false_str = "false";
    if (b) {
        return String_copy(&true_str);
    } else {
        return String_copy(&false_str);
    }
}

String Bool_format(const String *str, bool b) {
    String buffer;
    asprintf(&buffer, *str, b);
    return buffer;
}

String Char_str(char c) {
    String buffer;
    asprintf(&buffer, "%c", c);
    return buffer;
}

String Char_prn(char c) {
    String buffer;
    asprintf(&buffer, "\\%c", c);
    return buffer;
}

String Char_format(const String *str, char b) {
    String buffer;
    asprintf(&buffer, *str, b);
    return buffer;
}

String Double_str(double x) {
    String buffer;
    asprintf(&buffer, "%g", x);
    return buffer;
}

String Double_format(const String *s, double x) {
    String buffer;
    asprintf(&buffer, *s, x);
    return buffer;
}

String Float_str(float x) {
    String buffer;
    asprintf(&buffer, "%gf", x);
    return buffer;
}

String Float_format(const String *str, float x) {
    String buffer;
    asprintf(&buffer, *str, x);
    return buffer;
}

String Int_str(int x) {
    String buffer;
    asprintf(&buffer, "%d", x);
    return buffer;
}

String Int_format(const String *str, int x) {
    String buffer;
    asprintf(&buffer, *str, x);
    return buffer;
}

int Int_from_MINUS_string(const String *s) {
    return atoi(*s);
}

String Long_str(long x) {
    String buffer;
    asprintf(&buffer, "%ldl", x);
    return buffer;
}

String Long_format(const String *str, long x) {
    String buffer;
    asprintf(&buffer, *str, x);
    return buffer;
}

long Long_from_MINUS_string(const String *s) {
    return atol(*s);
}

String Byte_str(uint8_t x) {
    String buffer;
    asprintf(&buffer, "%ub", x);
    return buffer;
}

String Byte_format(const String* str, uint8_t x) {
    String buffer;
    asprintf(&buffer, *str, x);
    return buffer;
}

uint8_t Byte_from_MINUS_string(const String *s) {
    return atoi(*s);
}

int String_index_MINUS_of_MINUS_from(const String *s, char c, int i) {
    /* Return index of first occurrence of `c` in `s` AFTER index i
     * Returns -1 if not found
     */
    ++i;  // skip first character as we want AFTER i
    int len = strlen(*s);
    for (; i < len; ++i) {
        if (c == (*s)[i]) {
            return i;
        }
    }
    return -1;
}

int String_index_MINUS_of(const String *s, char c) {
    /* Return index of first occurrence of `c` in `s`
     * Returns -1 if not found
     */
    return String_index_MINUS_of_MINUS_from(s, c, -1);
}
