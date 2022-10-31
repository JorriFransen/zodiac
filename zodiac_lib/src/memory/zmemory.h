#pragma once

#include <common.h>
#include <defines.h>

#include <stdlib.h>
#include <cstring>

namespace Zodiac
{

ZAPI ZINLINE void* zallocate(i64 size)
{
    assert(size);
    return malloc(size);
}

ZAPI ZINLINE void* zallocate_aligned(i64 size, i64 alignment)
{
    assert(size && alignment);
    assert(is_power_of_two(alignment));

    if (alignment == 1) {
        return zallocate(size);
    } else {
        auto am1 = alignment - 1;

        auto padded_size = size + am1 + sizeof(void *);
        void *memory = malloc(padded_size);

        void *ptr = (void *)(((i64)memory + am1 + sizeof(void *)) & ~(am1));
        ((void **)ptr)[-1] = (void*)memory;

        return ptr;
    }
}

ZAPI ZINLINE void zfree(void *memory, i64 size)
{
    assert(memory && size);
    ::free(memory);
}

ZAPI ZINLINE void zfree_aligned(void *memory, i64 size, i64 alignment)
{
    assert(memory && size && alignment);
    assert(is_power_of_two(alignment));

    if (alignment == 1) {
        zfree(memory, size);
    } else {
        auto ptr = ((void **)memory)[-1];
        zfree(ptr, size);
    }

}

ZAPI ZINLINE void* zmemset(void *memory, i64 value, i64 num)
{
    assert(memory);
    return memset(memory, value, num);
}

ZAPI ZINLINE void* zzeromem(void *memory, i64 num)
{
    assert(memory);
    return zmemset(memory, 0, num);
}

ZAPI ZINLINE void* zmemcpy(void *dest, const void *src, i64 num)
{
    return memcpy(dest, src, num);
}

ZAPI ZINLINE i64 zmemcmp(const void *a, const void *b, i64 num)
{
    assert(a && b);
    return memcmp(a, b, num);
}

}
