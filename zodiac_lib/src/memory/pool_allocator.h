#pragma once

#include <defines.h>

namespace Zodiac
{

struct Pool_Chunk
{
    Pool_Chunk *next;
};

struct Pool_Allocator
{
    u32 capacity;
    u32 element_size;
    void *memory;

    Pool_Chunk *head;

    bool owns_memory;
};

ZAPI void pool_allocator_create(u32 capacity, u32 element_size, void *memory, Pool_Allocator *out_allocator);
ZAPI void pool_allocator_destroy(Pool_Allocator *allocator);

ZAPI void *pool_allocator_allocate(Pool_Allocator *allocator);
ZAPI void pool_allocator_free(Pool_Allocator *allocator, void *ptr);
ZAPI u32 pool_allocator_free_capacity(Pool_Allocator *pool);

}
