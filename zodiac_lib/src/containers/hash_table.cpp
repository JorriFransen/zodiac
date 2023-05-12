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
    return hash_s64(key);
}

}