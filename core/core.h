#if defined(WIN32) || defined(_WIN32) || \
    defined(__WIN32) && !defined(__CYGWIN__)
#include <BaseTsd.h>
#include <windows.h>
typedef SSIZE_T ssize_t;
#endif
#ifndef _WIN32
#include <unistd.h>
#endif
#include <inttypes.h>

typedef char *String;
typedef char *Pattern;
typedef int64_t Long;

#if defined NDEBUG
#define CHK_INDEX(i, n)
#else

#define CHK_INDEX_FORMAT_STRING ":%u: bad index: %zd < %zd\n"

#define CHK_INDEX(i, n)                                                    \
    do {                                                                   \
        size_t __si = (size_t)i;                                           \
        size_t __ni = (size_t)n;                                           \
        if (!(__si < __ni)) {                                              \
            printf(__FILE__ CHK_INDEX_FORMAT_STRING, __LINE__, (ssize_t)i, \
                   (ssize_t)n);                                            \
            abort();                                                       \
        }                                                                  \
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
