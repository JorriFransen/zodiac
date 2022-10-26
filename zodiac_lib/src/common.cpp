#include "common.h"

#include <cassert>

namespace Zodiac
{

uint64_t hash_c_string(const char *cstr, int64_t length)
{
    // 64 bit FNV hash
    if (length == 0)
    {
        return 0;
    }

    uint64_t hash = 14695981039346656037u;

    for (int64_t i = 0; i < length; i++)
    {
        hash = hash ^ (cstr[i]);
        hash = hash * 1099511628211;
    }

    return hash;

}

uint64_t hash_mix(uint64_t a, uint64_t b)
{
    uint64_t string[2] = { a, b };
    assert(sizeof(string) == 16);

    return hash_c_string((const char *)&string, sizeof(string));
}

}