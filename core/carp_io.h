#include <sys/stat.h>
#include <errno.h>
#ifdef _WIN32
#include <direct.h>
#define getcwd _getcwd
#define chdir _chdir
#define stat _stat
#define S_ISREG(m) (((m) & _S_IFMT) == _S_IFREG)
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#else
#include <dirent.h>
#include <unistd.h>
#include <sys/param.h>
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

bool IO_Raw_file_MINUS_exists_QMARK_(const String* path) {
    struct stat st;
    if (stat(*path, &st) == 0) {
        return S_ISREG(st.st_mode);
    }
    return false;
}

bool IO_Raw_dir_MINUS_exists_QMARK_(const String* path) {
    struct stat st;
    if (stat(*path, &st) == 0) {
        return S_ISDIR(st.st_mode);
    }
    return false;
}

#ifdef _WIN32
int IO_Raw_list_MINUS_dir(const String* path, Array* out_result) {
    WIN32_FIND_DATA fd;
    char search_path[MAX_PATH];
    snprintf(search_path, MAX_PATH, "%s\\*", *path);
    HANDLE hFind = FindFirstFile(search_path, &fd);
    if (hFind != INVALID_HANDLE_VALUE) {
        out_result->len = 0;
        out_result->capacity = 0;
        out_result->data = NULL;
        do {
            if (strcmp(fd.cFileName, ".") == 0 || strcmp(fd.cFileName, "..") == 0) {
                continue;
            }
            String name = String_from_MINUS_cstr(fd.cFileName);
            out_result->len++;
            out_result->data = CARP_REALLOC(out_result->data, sizeof(String) * out_result->len);
            ((String*)out_result->data)[out_result->len - 1] = name;
        } while (FindNextFile(hFind, &fd));
        FindClose(hFind);
        return 0;
    } else {
        return -1;
    }
}
#else
int IO_Raw_list_MINUS_dir(const String* path, Array* out_result) {
    DIR *d;
    struct dirent *dir;
    d = opendir(*path);
    if (d) {
        out_result->len = 0;
        out_result->capacity = 0;
        out_result->data = NULL;
        while ((dir = readdir(d)) != NULL) {
            if (strcmp(dir->d_name, ".") == 0 || strcmp(dir->d_name, "..") == 0) {
                continue;
            }
            String name = String_from_MINUS_cstr(dir->d_name);
            out_result->len++;
            out_result->data = CARP_REALLOC(out_result->data, sizeof(String) * out_result->len);
            ((String*)out_result->data)[out_result->len - 1] = name;
        }
        closedir(d);
        return 0;
    } else {
        return -1;
    }
}
#endif

int IO_Raw_mkdir(const String* path) {
#ifdef _WIN32
    return _mkdir(*path);
#else
    return mkdir(*path, 0755);
#endif
}

int IO_Raw_rmdir(const String* path) {
#ifdef _WIN32
    return _rmdir(*path);
#else
    return rmdir(*path);
#endif
}

String IO_Raw_get_MINUS_cwd() {
    char buffer[4096];
    if (getcwd(buffer, sizeof(buffer))) {
        return String_from_MINUS_cstr(buffer);
    }
    return NULL;
}

int IO_Raw_set_MINUS_cwd(const String* path) {
    return chdir(*path);
}
