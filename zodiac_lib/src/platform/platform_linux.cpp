#include "platform.h"

#ifdef ZPLATFORM_LINUX

#include <common.h>

#include <cstring>
#include <stdlib.h>

namespace Zodiac
{

void *platform_allocate(u64 size, u64 alignment/*=1*/)
{
    assert(size && alignment);
    assert(is_power_of_two(alignment));

    if (alignment == 1) {
        return malloc(size);
    } else {
        auto am1 = alignment - 1;

        auto padded_size = size + am1 + sizeof(void *);
        void *memory = malloc(padded_size);

        void *ptr = (void *)(((i64)memory + am1 + sizeof(void *)) & ~(am1));
        ((void **)ptr)[-1] = (void*)memory;

        return ptr;
    }
}

void platform_free(void *memory, u64 size, u64 alignment/*=1*/)
{
    assert(memory && size && alignment);
    assert(is_power_of_two(alignment));

    if (alignment == 1) {
        ::free(memory);
    } else {
        auto ptr = ((void **)memory)[-1];
        ::free(ptr);
    }
}

void *platform_memset(void *memory, i64 value, u64 size)
{
    assert(memory && size);
    return memset(memory, value, size);
}

void *platform_zero_mem(void *memory, u64 num)
{
    assert(memory);
    return platform_memset(memory, 0, num);
}

void *platform_memcpy(void *dest, const void *src, u64 num)
{
    assert(dest && src);
    return memcpy(dest, src, num);
}

i64 platform_memcmp(const void *a, const void *b, u64 num)
{
    assert(a && b);
    return memcmp(a, b, num);
}

}

#endif

