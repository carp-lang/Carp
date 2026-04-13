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

/* Raw file descriptor operations. */
#ifdef _WIN32
#include <io.h>
#include <sys/stat.h>
#else
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

int IO_Raw_open(String* path, int flags) {
#ifdef _WIN32
    return _open(*path, flags);
#else
    return open(*path, flags);
#endif
}

int IO_Raw_close_fd(int fd) {
#ifdef _WIN32
    return _close(fd);
#else
    return close(fd);
#endif
}

Long IO_Raw_fstat_size(int fd) {
#ifdef _WIN32
    struct _stat64 st;
    if (_fstat64(fd, &st) == -1) return -1;
#else
    struct stat st;
    if (fstat(fd, &st) == -1) return -1;
#endif
    return (Long)st.st_size;
}

int IO_Raw_fileno(void* fp) {
#ifdef _WIN32
    return _fileno((FILE*)fp);
#else
    return fileno((FILE*)fp);
#endif
}

