void IO_println(String *s) {
    puts(*s);
}
void IO_print(String *s) {
    printf("%s", *s);
}

void IO_errorln(String *s) {
    fprintf(stderr, "%s\n", *s);
}
void IO_error(String *s) {
    fprintf(stderr, "%s", *s);
}

String IO_get_MINUS_line() {
    char *b = NULL;
    size_t cap = 64;
    size_t sofar = 0;
    while (1) {
        b = CARP_REALLOC(b, cap);
        b[cap - 1] = -1;
        if (!fgets(b + sofar, cap - sofar, stdin)) break;
        sofar = cap - 1;
        if (b[cap - 1] == -1) break;
        cap *= 2;
    }
    return b;
}

String IO_unsafe_MINUS_read_MINUS_file(const String *filename) {
    String buffer = 0;
    Long length;
    FILE *f = fopen(*filename, "rb");

    if (f) {
        fseek(f, 0, SEEK_END);
        length = ftell(f);
        fseek(f, 0, SEEK_SET);
        buffer = CARP_MALLOC(length + 1);
        if (buffer) {
            size_t sz = fread(buffer, 1, length, f);
            assert(sz == length);
            buffer[sz] = '\0';
        } else {
            fprintf(stderr, "Failed to open buffer from file: %s\n", *filename);
            buffer = String_empty();
        }
        fclose(f);
    } else {
        fprintf(stderr, "Failed to open file: %s\n", *filename);
        buffer = String_empty();
    }

    return buffer;
}
