#pragma once

#include <defines.h>

#include "allocator.h"
#include "dynamic_allocator.h"
#include "platform/platform.h"
#include "util/asserts.h"

namespace Zodiac
{

ZAPI extern bool memory_system_initialized;
ZAPI extern Dynamic_Allocator dynamic_allocator_state;
ZAPI extern Allocator dynamic_allocator;

ZAPI bool memory_system_initialize();

ZAPI ZINLINE void *zallocate(u64 size)
{
    assert(memory_system_initialized);
    assert(size);
    return alloc(&dynamic_allocator, size);
}

template <typename T>
ZAPI ZINLINE T *zallocate()
{
    return (T *)zallocate(sizeof(T));
}

ZAPI ZINLINE void* zallocate_aligned(u64 size, u64 alignment)
{
    assert(memory_system_initialized);
    assert(size && alignment);
    assert(false && !"zalloate_aligned is not supported yet...");
    return nullptr;
}

ZAPI ZINLINE void zfree(void *memory)
{
    assert(memory_system_initialized);
    assert(memory);
    return free(&dynamic_allocator, memory);
}

ZAPI ZINLINE void zfree_aligned(void *memory, u64 size, u64 alignment)
{
    assert(memory_system_initialized);
    assert(memory && size && alignment);
    assert(false && !"zfree_aligned is not supported yet...");
    assert(false);
}

ZAPI ZINLINE void* zmemset(void *memory, u64 value, u64 num)
{
    assert(memory);
    return platform_memset(memory, value, num);
}

ZAPI ZINLINE void* zzeromem(void *memory, u64 num)
{
    assert(memory);
    return zmemset(memory, 0, num);
}

ZAPI ZINLINE void* zmemcpy(void *dest, const void *src, u64 num)
{
    return platform_memcpy(dest, src, num);
}

ZAPI ZINLINE void *zmemmove(void *dest, const void *src, u64 n)
{
    return platform_memmove(dest, src, n);
}

ZAPI ZINLINE u64 zmemcmp(const void *a, const void *b, u64 num)
{
    assert(a && b);
    return platform_memcmp(a, b, num);
}

}
