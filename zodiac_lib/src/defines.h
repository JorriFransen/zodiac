#pragma once

#include <cassert>
#include <stdio.h>
#include <stdlib.h>
#include <cstdint>

#if (defined(__clang__) || defined(__gcc__)) && (defined(__STDC_VERSION__)  && __STDC_VERSION__ > 201112L)
#define STATIC_ASSERT _Static_assert
#else
#define STATIC_ASSERT static_assert
#endif

#define zodiac_assert_fatal(cond, err) \
    if (!(cond)) { \
        assert(cond); \
        fprintf(stderr, "%s:%d: Assertion failed: %s", __FILE__, __LINE__, (err)); \
        exit(42); \
    }

#define zodiac_info(info) \
    fprintf(stderr, "%s:%d: Info: %s\n", __FILE__, __LINE__, (info));

#define zodiac_warn(err) \
    fprintf(stderr, "%s:%d: Warning: %s\n", __FILE__, __LINE__, (err));

#if defined(__clang__) || defined(__gcc__)
#define ZINLINE __attribute__((always_inline)) inline
#define ZNOINLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define ZINLINE __forceinline
#define ZNOINLINE __declspec(noinline)
#else
#define ZINLINE static inline
#define ZNOINLINE
#endif


typedef unsigned char       u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef signed char       i8;
typedef signed short     i16;
typedef signed int       i32;
typedef signed long long i64;

typedef  i8  s8;
typedef i16 s16;
typedef i32 s32;
typedef i64 s64;

typedef float  r32;
typedef double r64;

#define U64_MAX UINT64_MAX
#define U32_MAX UINT32_MAX
#define U16_MAX UINT16_MAX
#define U8_MAX UINT8_MAX

#define I64_MAX INT64_MAX
#define I32_MAX INT32_MAX
#define I16_MAX INT16_MAX
#define I8_MAX INT8_MAX

STATIC_ASSERT(sizeof(u8) == 1, "Expected sizeof(u8) to be 1 byte");
STATIC_ASSERT(sizeof(u16) == 2, "Expected sizeof(u16) to be 2 bytes");
STATIC_ASSERT(sizeof(u32) == 4, "Expected sizeof(u32) to be 4 bytes");
STATIC_ASSERT(sizeof(u64) == 8, "Expected sizeof(u64) to be 8 bytes");

STATIC_ASSERT(sizeof(i8) == 1, "Expected sizeof(i8) to be 1 byte");
STATIC_ASSERT(sizeof(i16) == 2, "Expected sizeof(i16) to be 2 bytes");
STATIC_ASSERT(sizeof(i32) == 4, "Expected sizeof(i32) to be 4 bytes");
STATIC_ASSERT(sizeof(i64) == 8, "Expected sizeof(i64) to be 8 bytes");

STATIC_ASSERT(sizeof(s8) == 1, "Expected sizeof(s8) to be 1 byte");
STATIC_ASSERT(sizeof(s16) == 2, "Expected sizeof(s16) to be 2 bytes");
STATIC_ASSERT(sizeof(s32) == 4, "Expected sizeof(s32) to be 4 bytes");
STATIC_ASSERT(sizeof(s64) == 8, "Expected sizeof(s64) to be 8 bytes");

STATIC_ASSERT(sizeof(r32) == 4, "Expected sizeof(r32) to be 4 bytes");
STATIC_ASSERT(sizeof(r64) == 8, "Expected sizeof(r64) to be 8 bytes");



#define GIBIBYTE(x) (x * 1024 * 1024 * 1024)
#define MEBIBYTE(x) (x * 1024 * 1024)
#define KIBIBYTE(x) (x * 1024)

#define GIGABYTE(x) (x * 1000 * 1000 * 1000)
#define MEGABYTE(x) (x * 1000 * 1000)
#define KILOBYTE(x) (x * 1000)

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)

#define ZPLATFORM_WINDOWS 1
#ifndef _WIN64
#error "64-bit is required on Windows!"
#endif // _WIN64

#elif defined(__linux__) || defined(__gnu_linux__)

//STATIC_ASSERT(false, "Unsupported platform (linux).");
#define ZPLATFORM_LINUX 1

#elif defined(__unix__)

#define ZPLATFORM_UNIX 1
STATIC_ASSERT(false, "Unsupported platform (unix).");

#elif __APPLE__

#define ZPLATFORM_APPLE 1
STATIC_ASSERT(false, "Unsupported platform (Apple).");

#endif // defined(WIN32) || defined(_WIN32) || defined(__WIN32__)

#ifdef ZEXPORT

// Exports
#ifdef _MSC_VER
#define ZAPI __declspec(dllexport)
#else
#define ZAPI __attribute__((visibility("default")))
#endif
#else
// Imports
#ifdef _MSC_VER
#define ZAPI __declspec(dllimport)
#else
#define ZAPI
#endif

#endif // ZEXPORT

ZINLINE u64 get_aligned(u64 operand, u64 alignment) {
    return ((operand + (alignment - 1)) & ~(alignment - 1));
}
