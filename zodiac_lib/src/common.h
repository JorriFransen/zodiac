#pragma once

#include <cstdint>

namespace Zodiac
{

#define KB(x) (x * 1024)
#define MB(x) (KB(x) * 1024)

#define zodiac_assert_fatal(cond, err) \
    if (!(cond)) { \
        assert((cond) && !(err)); \
        fprintf(stderr, "%s:%d: Assertion failed: %s", __FILE__, __LINE__, (err)); \
        exit(42); \
    }

template <typename T>
T min(T a, T b) {
    if (a < b) return a;
    return b;
}

union Integer_Value
{
    int64_t s64;
    uint64_t u64;
    int32_t s32;
    uint32_t u32;
    int16_t s16;
    uint16_t u16;
    int8_t s8;
    uint8_t u8;
};

struct Real_Value
{
    float r32;
    double r64;
};

uint64_t hash_c_string(const char *cstr, int64_t length);
uint64_t hash_mix(uint64_t a, uint64_t b);

}