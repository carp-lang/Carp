void IO_println(String* s) {
    puts(*s);
}
void IO_print(String* s) {
    printf("%s", *s);
}

void IO_errorln(String* s) {
    fprintf(stderr, "%s\n", *s);
}
void IO_error(String* s) {
    fprintf(stderr, "%s", *s);
}

String IO_get_MINUS_line() {
    char* b = NULL;
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

String IO_unsafe_MINUS_read_MINUS_file(const String* filename) {
    String buffer = 0;
    Long length;
    FILE* f = fopen(*filename, "rb");

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

Array IO_Raw_read_MINUS_lines(const String* filename) {
    FILE* f = fopen(*filename, "r");
    Array result = {0, 0, NULL};
    if (f) {
        char buffer[4096];
        char* line = NULL;
        size_t line_len = 0;
        
        while (fgets(buffer, sizeof(buffer), f)) {
            size_t buf_len = strlen(buffer);
            line = CARP_REALLOC(line, line_len + buf_len + 1);
            strcpy(line + line_len, buffer);
            line_len += buf_len;
            
            if (line[line_len - 1] == '\n') {
                line[line_len - 1] = '\0'; // Strip newline
                result.len++;
                result.data = CARP_REALLOC(result.data, sizeof(String) * result.len);
                String s = CARP_MALLOC(line_len);
                strcpy(s, line);
                ((String*)result.data)[result.len - 1] = s;
                
                // Reset for next line
                line_len = 0;
                line[0] = '\0';
            }
        }
        
        // Handle last line if it didn't end with a newline
        if (line_len > 0) {
            result.len++;
            result.data = CARP_REALLOC(result.data, sizeof(String) * result.len);
            String s = CARP_MALLOC(line_len + 1);
            strcpy(s, line);
            ((String*)result.data)[result.len - 1] = s;
        }
        
        if (line) CARP_FREE(line);
        fclose(f);
    } else {
        result.len = -1;
    }
    return result;
}
