#include "common.h"

#include "util/asserts.h"

namespace Zodiac
{

u64 hash_c_string(const char *cstr, u64 length)
{
    // 64 bit FNV hash
    if (length == 0)
    {
        return 0;
    }

    u64 hash = 14695981039346656037u;

    for (u64 i = 0; i < length; i++)
    {
        hash = hash ^ ((u64)cstr[i]);
        hash = hash * 1099511628211;
    }

    return hash;

}

u64 hash_mix(u64 a, u64 b)
{
    u64 string[2] = { a, b };
    assert(sizeof(string) == 16);

    return hash_c_string((const char *)&string, sizeof(string));
}

}
