#include "platform.h"

#ifdef ZPLATFORM_LINUX

#include <common.h>

#include <cstring>
#include <stdlib.h>

namespace Zodiac
{

struct Linear_Alloc_Header
{
    void *start; // NOTE: Since the size is stored in 32 bits, we should be able to store the lower 32 bits of the start addess, and replace those in the pointer passed to free.
    u32 size;
    u16 alignment;
};

void *platform_allocate(u64 size, u64 alignment/*=1*/)
{
    assert(size && alignment);
    assert(is_power_of_two(alignment));

    u64 total_size = size + alignment - 1 + sizeof(Linear_Alloc_Header);
    void *memory = malloc(total_size);
    assert(memory);
    u64 aligned_offset = get_aligned(((u64)memory) + sizeof(Linear_Alloc_Header), alignment);
    auto header = (Linear_Alloc_Header *)(aligned_offset - sizeof(Linear_Alloc_Header));
    header->start = memory;
    header->size = size;
    header->alignment = alignment;

    return (void *)aligned_offset;
}

void platform_free(void *memory)
{
    assert(memory);

    auto header = (Linear_Alloc_Header *)((u64)memory - sizeof(Linear_Alloc_Header));
    ::free(header->start);
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

