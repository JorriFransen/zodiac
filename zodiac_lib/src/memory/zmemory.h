#pragma once


#include <common.h>
#include <platform/platform.h>

namespace Zodiac
{

ZAPI ZINLINE void* zallocate(i64 size)
{
    return platform_allocate(size);
}

ZAPI ZINLINE void* zallocate_aligned(i64 size, i64 alignment)
{
    return platform_allocate(size, alignment);
}

ZAPI ZINLINE void zfree(void *memory, i64 size)
{
    assert(memory && size);
    platform_free(memory, size);
}

ZAPI ZINLINE void zfree_aligned(void *memory, i64 size, i64 alignment)
{
    assert(memory && size && alignment);
    platform_free(memory, size, alignment);
}

ZAPI ZINLINE void* zmemset(void *memory, i64 value, i64 num)
{
    assert(memory);
    return platform_memset(memory, value, num);
}

ZAPI ZINLINE void* zzeromem(void *memory, i64 num)
{
    assert(memory);
    return zmemset(memory, 0, num);
}

ZAPI ZINLINE void* zmemcpy(void *dest, const void *src, i64 num)
{
    return platform_memcpy(dest, src, num);
}

ZAPI ZINLINE i64 zmemcmp(const void *a, const void *b, i64 num)
{
    assert(a && b);
    return platform_memcmp(a, b, num);
}

}
