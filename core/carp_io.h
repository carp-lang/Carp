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

char IO_EOF = (char)EOF;

#ifdef _WIN32
// getline isn't a C standard library function so it's missing on windows
// This implementation is stolen from StackOverflow, not sure if it's optimal...
size_t getline(char **lineptr, size_t *n, FILE *stream) {
    size_t pos;
    int c;

    if (lineptr == NULL || stream == NULL || n == NULL) {
        errno = EINVAL;
        return -1;
    }

    c = fgetc(stream);
    if (c == EOF) {
        return -1;
    }

    if (*lineptr == NULL) {
        *lineptr = malloc(128);
        if (*lineptr == NULL) {
            return -1;
        }
        *n = 128;
    }

    pos = 0;
    while (c != EOF) {
        if (pos + 1 >= *n) {
            size_t new_size = *n + (*n >> 2);
            if (new_size < 128) {
                new_size = 128;
            }
            char *new_ptr = CARP_REALLOC(*lineptr, new_size);
            if (new_ptr == NULL) {
                return -1;
            }
            *n = new_size;
            *lineptr = new_ptr;
        }

        ((unsigned char *)(*lineptr))[pos++] = c;
        if (c == '\n') {
            break;
        }
        c = fgetc(stream);
    }

    (*lineptr)[pos] = '\0';
    return pos;
}
#endif

String IO_get_MINUS_line() {
    size_t size = 1024;
    String buffer = CARP_MALLOC(size);
    getline(&buffer, &size, stdin);
    return buffer;
}

String IO_read_MINUS_file(const String *filename) {
    String buffer = 0;
    long length;
    FILE *f = fopen(*filename, "rb");

    if (f) {
        fseek(f, 0, SEEK_END);
        length = ftell(f);
        fseek(f, 0, SEEK_SET);
        buffer = CARP_MALLOC(length + 1);
        if (buffer) {
            fread(buffer, 1, length, f);
            buffer[length] = '\0';
        } else {
            printf("Failed to open buffer from file: %s\n", *filename);
            buffer = String_empty();
        }
        fclose(f);
    } else {
        printf("Failed to open file: %s\n", *filename);
        buffer = String_empty();
    }

    return buffer;
}

char IO_fgetc(FILE *f) {
    return (char)fgetc(f);
}

void IO_fclose(FILE *f) {
    fclose(f);
}

FILE *IO_fopen(const String *filename, const String *mode) {
    return fopen(*filename, *mode);
}
