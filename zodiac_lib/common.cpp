#include "common.h"

#include "util/asserts.h"
#include "util/zstring.h"

namespace Zodiac
{

u64 hash_string(const char *cstr, u64 length)
{
    u64 hash = 14695981039346656037u;

    for (u64 i = 0; i < length; i++)
    {
        hash = hash ^ ((u64)cstr[i]);
        hash = hash * 1099511628211;
    }

    return hash;
}

u64 hash_string(const char *cstr)
{
    return hash_string(cstr, zstrlen(cstr));
}

u64 hash_s64(s64 key)
{

    if (key == 0) key++;

    key = (~key) + (key << 21); // key = (key << 21) - key - 1;
    key = key ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); // key * 265
    key = key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); // key * 21
    key = key ^ (key >> 28);
    key = key + (key << 31);

    assert(key);

    return key;
}

u64 hash_mix(u64 a, u64 b)
{
    u64 string[2] = { a, b };
    assert(sizeof(string) == 16);

    return hash_string((const char *)&string, sizeof(string));
}

}
