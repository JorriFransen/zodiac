#include "hash_table.h"

#include <cstring>

namespace Zodiac
{

u64 hash_key(const char *str)
{
    auto length = strlen(str);

    // 64 bit FNV hash
    if (length == 0)
    {
        return 0;
    }

    u64 hash = 14695981039346656037u;

    for (s64 i = 0; i < length; i++)
    {
        hash = hash ^ (str[i]);
        hash = hash * 1099511628211;
    }

    return hash;
}

u64 hash_key(s64 key)
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

}