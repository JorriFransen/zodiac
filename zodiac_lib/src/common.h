#pragma once

#include "defines.h"

namespace Zodiac
{

template <typename T>
T min(T a, T b) {
    if (a < b) return a;
    return b;
}

template <typename T>
T max(T a, T b) {
    if (a > b) return a;
    return b;
}

union Integer_Value
{
    i64 s64;
    u64 u64;
    i32 s32;
    u32 u32;
    i16 s16;
    u16 u16;
    i8 s8;
    u8 u8;
};

struct Real_Value
{
    float r32;
    double r64;
};

template <typename T>
ZAPI ZINLINE bool is_power_of_two(T n) {
    return n && (!(n & (n-1)));
}

ZAPI u64 hash_s64(s64 key);

ZAPI u64 hash_string(const char *cstr, u64 length);
ZAPI u64 hash_string(const char *cstr);
ZAPI u64 hash_mix(u64 a, u64 b);

}
