#if defined _WIN32

#ifdef __TINYC__
// missing definitions for tcc-0.9.27-win32-bin.zip => 32bit TCC
#define CP_UTF8 65001
#define strtof strtod
#endif

#include <windows.h>
#if !defined __CYGWIN__ && !defined __MINGW32__
typedef intptr_t ssize_t;
#endif
#endif
#ifndef _WIN32
#include <unistd.h>
#endif
#include <inttypes.h>
#include <locale.h>

typedef char* String;
typedef char* Pattern;
typedef int64_t Long;
typedef uint32_t Char;
typedef char CChar;
typedef void* c_code;

#if defined NDEBUG
#define CHK_INDEX(i, n)
#define CHK_FORMAT(s)
#else

#define CHK_INDEX(i, n)                                                    \
    do {                                                                   \
        size_t __si = (size_t)i;                                           \
        size_t __ni = (size_t)n;                                           \
        if (!(__si < __ni)) {                                              \
            printf(__FILE__ ":%u: bad index: %zu < %zu\n", __LINE__, __ni, \
                   __si);                                                  \
            abort();                                                       \
        }                                                                  \
    } while (0)

/* The 'unsafe-format' implementations forward to snprintf with exactly one
 * value, so their format string must carry exactly one directive. An escaped
 * '%%' consumes no value and does not count. */
#define CHK_FORMAT(s)                                                         \
    do {                                                                      \
        const char* __fs = (s);                                               \
        const char* __fp = __fs;                                              \
        size_t __fn = 0;                                                      \
        int __ft = 0;                                                         \
        for (; *__fp; __fp++) {                                               \
            if (*__fp != '%') continue;                                       \
            if (__fp[1] == '%') {                                             \
                __fp++;                                                       \
                continue;                                                     \
            }                                                                 \
            if (__fp[1] == '\0') {                                            \
                __ft = 1;                                                     \
                break;                                                        \
            }                                                                 \
            __fn++;                                                           \
        }                                                                     \
        if (__ft) {                                                           \
            printf(__FILE__                                                   \
                   ":%u: bad format string: trailing '%%' in \"%s\"\n",       \
                   __LINE__, __fs);                                           \
            abort();                                                          \
        }                                                                     \
        if (__fn != 1) {                                                      \
            printf(__FILE__                                                   \
                   ":%u: bad format string: expected exactly one directive, " \
                   "found %zu in \"%s\"\n",                                   \
                   __LINE__, __fn, __fs);                                     \
            abort();                                                          \
        }                                                                     \
    } while (0)
#endif

#define UNHANDLED(file, line)                                                  \
    do {                                                                       \
        printf("Unhandled case in 'match' expression at %s:%d\n", file, line); \
        abort();                                                               \
    } while (0)

// Array
typedef struct {
    size_t len;
    size_t capacity;
    void* data;
} Array;

// Lambdas
typedef struct {
    void* callback;
    void* env;
    void* delete;
    void* copy;
} Lambda;

typedef void* LambdaEnv;
