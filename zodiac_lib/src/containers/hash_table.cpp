#include "hash_table.h"

#include "common.h"

#include <cstring>

namespace Zodiac
{

u64 hash_key(const char *str)
{
    return hash_c_string(str, strlen(str));
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