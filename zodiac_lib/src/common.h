#pragma once

#include <defines.h>

namespace Zodiac
{

template <typename T>
T min(T a, T b) {
    if (a < b) return a;
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

u64 hash_c_string(const char *cstr, i64 length);
u64 hash_mix(u64 a, u64 b);

}
