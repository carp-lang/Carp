#include <sys/stat.h>
#include <dirent.h>
#ifdef _WIN32
#include <direct.h>
#define getcwd _getcwd
#define chdir _chdir
#else
#include <unistd.h>
#endif

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

bool IO_file_MINUS_exists_QMARK_(const String* path) {
    struct stat st;
    if (stat(*path, &st) == 0) {
        return S_ISREG(st.st_mode);
    }
    return false;
}

bool IO_dir_MINUS_exists_QMARK_(const String* path) {
    struct stat st;
    if (stat(*path, &st) == 0) {
        return S_ISDIR(st.st_mode);
    }
    return false;
}

Array IO_Raw_list_MINUS_dir(const String* path) {
    DIR *d;
    struct dirent *dir;
    d = opendir(*path);
    Array result = {0, 0, NULL};
    if (d) {
        while ((dir = readdir(d)) != NULL) {
            if (strcmp(dir->d_name, ".") == 0 || strcmp(dir->d_name, "..") == 0) {
                continue;
            }
            String name = CARP_MALLOC(strlen(dir->d_name) + 1);
            strcpy(name, dir->d_name);
            result.len++;
            result.data = CARP_REALLOC(result.data, sizeof(String) * result.len);
            ((String*)result.data)[result.len - 1] = name;
        }
        closedir(d);
    } else {
        result.len = -1;
    }
    return result;
}

int IO_mkdir(const String* path) {
#ifdef _WIN32
    return _mkdir(*path);
#else
    return mkdir(*path, 0777);
#endif
}

int IO_rmdir(const String* path) {
#ifdef _WIN32
    return _rmdir(*path);
#else
    return rmdir(*path);
#endif
}

String IO_Raw_get_MINUS_cwd() {
    char* buffer = getcwd(NULL, 0);
    if (buffer) {
        String result = CARP_MALLOC(strlen(buffer) + 1);
        strcpy(result, buffer);
        free(buffer);
        return result;
    }
    return NULL;
}

int IO_Raw_set_MINUS_cwd(const String* path) {
    return chdir(*path);
}
