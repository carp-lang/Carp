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

typedef char *String;
typedef char *Pattern;
typedef int64_t Long;
typedef uint32_t Char;
typedef char CChar;
typedef void *c_code;

#if defined NDEBUG
#define CHK_INDEX(i, n)
#else

#define CHK_INDEX(i, n)                                                    \
    do {                                                                   \
        size_t __si = (size_t)i;                                           \
        size_t __ni = (size_t)n;                                           \
        if (!(__si < __ni)) {                                              \
            printf(__FILE__ ":%u: bad index: %zu < %zu\n", __LINE__, __ni, \
            __si);                                                         \
            abort();                                                       \
        }                                                                  \
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
    void *data;
} Array;

// Lambdas
typedef struct {
    void *callback;
    void *env;
    void *delete;
    void *copy;
} Lambda;

typedef void *LambdaEnv;
