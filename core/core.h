#if defined(WIN32) || defined(_WIN32) || \
    defined(__WIN32) && !defined(__CYGWIN__)
#include <windows.h>
#endif
#ifndef _WIN32
#include <unistd.h>
#endif

typedef char *String;
typedef char *Pattern;

#if defined NDEBUG
#define CHK_INDEX(i, n)
#else
#define CHK_INDEX(i, n)                                              \
    do {                                                             \
        size_t __si = (size_t)i;                                     \
        size_t __ni = (size_t)n;                                     \
        if (!(__si < __ni)) {                                        \
            printf(__FILE__ ":%u: bad index: %zd < %zd\n", __LINE__, \
                   (ssize_t)i, (ssize_t)n);                          \
            abort();                                                 \
        }                                                            \
    } while (0)
#endif

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
