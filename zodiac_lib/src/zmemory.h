#pragma once

#include <defines.h>

#include <stdlib.h>
#include <cstring>

namespace Zodiac
{

ZAPI ZINLINE void* zallocate(i64 size)
{
    return malloc(static_cast<size_t>(size));
}

ZAPI ZINLINE void zfree(void *memory, i64 size)
{
    assert(size > 0);
    ::free(memory);
}


ZAPI ZINLINE void* zmemset(void *memory, i64 value, i64 num)
{
    return memset(memory, value, num);
}

ZAPI ZINLINE void* zzeromem(void *memory, i64 num)
{
    return zmemset(memory, 0, num);
}

ZAPI ZINLINE void* zmemcpy(void *dest, const void *src, i64 num)
{
    return memcpy(dest, src, num);
}

ZAPI ZINLINE i64 zmemcmp(const void *a, const void *b, i64 num)
{
    return memcmp(a, b, num);
}

}
