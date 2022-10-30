#include "zmemory.h"

#include <cstdlib>
#include <cstring>

namespace Zodiac
{

void* zallocate(i64 size)
{
    return malloc(static_cast<size_t>(size));
}

void zfree(void *memory, i64 size)
{
    assert(size > 0);
    free(memory);
}

void* zmemset(void *memory, i64 value, i64 num)
{
    return memset(memory, value, num);
}

void* zzeromem(void *memory, i64 num)
{
    return zmemset(memory, 0, num);
}

void* zmemcpy(void *dest, const void *src, i64 num)
{
    return memcpy(dest, src, num);
}

i64 zmemcmp(const void *a, const void *b, i64 num)
{
    return memcmp(a, b, num);
}

}
