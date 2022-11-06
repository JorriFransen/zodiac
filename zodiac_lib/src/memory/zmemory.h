#pragma once


#include <common.h>
#include <memory/allocator.h>
#include <memory/dynamic_allocator.h>
#include <platform/platform.h>

namespace Zodiac
{

static bool memory_system_initialized = false;
static Dynamic_Allocator_State dynamic_allocator_state;
static Allocator dynamic_allocator;

ZAPI void memory_system_initialize();

ZAPI ZINLINE void* zallocate(i64 size)
{
    assert(memory_system_initialized);
    assert(size);
    assert(false);
    return nullptr;
}

ZAPI ZINLINE void* zallocate_aligned(i64 size, i64 alignment)
{
    assert(memory_system_initialized);
    assert(size && alignment);
    assert(false);
    return nullptr;
}

ZAPI ZINLINE void zfree(void *memory, i64 size)
{
    assert(memory_system_initialized);
    assert(memory && size);
    assert(false);
}

ZAPI ZINLINE void zfree_aligned(void *memory, i64 size, i64 alignment)
{
    assert(memory_system_initialized);
    assert(memory && size && alignment);
    assert(false);
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
