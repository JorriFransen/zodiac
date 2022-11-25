#pragma once

#include <defines.h>

#define ZASSERTS_ENABLED

#include <signal.h>

#ifdef ZASSERTS_ENABLED

#if _MSC_VER
#else //_MSC_VER

#define ZODIAC_ABORT() raise(SIGABRT)
#define ZODIAC_DEBUG_BREAK() __builtin_trap()

#endif //_MSC_VER

namespace Zodiac
{


ZAPI void report_assert_fail(const char* expression, const char* message, const char *file, i64 line);

#define assert(expr)                                       \
{                                                          \
    if (expr) {                                            \
    } else {                                               \
        report_assert_fail(#expr, "", __FILE__, __LINE__); \
        ZODIAC_ABORT();                                    \
    }                                                      \
}

#define assert_msg(expr, msg)                               \
{                                                           \
    if (expr) {                                             \
    } else {                                                \
        report_assert_fail(#expr, msg, __FILE__, __LINE__); \
        ZODIAC_ABORT();                                     \
    }                                                       \
}

#ifdef _DEBUG

#define debug_assert(expr)                                 \
{                                                          \
    if (expr) {                                            \
    } else {                                               \
        report_assert_fail(#expr, "", __FILE__, __LINE__); \
        ZODIAC_DEBUG_BREAK();                              \
    }                                                      \
}

#define debug_assert_msg(expr, msg)                         \
{                                                           \
    if (expr) {                                             \
    } else {                                                \
        report_assert_fail(#expr, msg, __FILE__, __LINE__); \
        ZODIAC_DEBUG_BREAK();                               \
    }                                                       \
}

#else //_DEBUG

#define debug_assert(expr)
#define debug_assert_msg(expr, msg)

#endif //_DEBUG

} //namespace Zodiac

#else // ZASSERTS_ENABLED

#define assert(expr)
#define assert_msg(expr)
#define debug_assert(expr)
#define debug_assert_msg(expr, msg)

#endif // ZASSERTS_ENABLED
