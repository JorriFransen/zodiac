#pragma once

#include "allocator.h"
#include "defines.h"

namespace Zodiac
{

struct Pool_Chunk
{
    Pool_Chunk* next;
};

struct Pool_Block
{
    Pool_Block *next;
    void *memory;
    Pool_Chunk *head;
};

struct Pool_Allocator
{
    u32 element_size;
    u32 block_capacity;

    Pool_Block *first_block;
    Pool_Block *current_block;
};

ZAPI void pool_allocator_create(u32 block_capacity, u32 element_size, Pool_Allocator *out_allocator);
ZAPI void pool_allocator_destroy(Pool_Allocator *allocator);
ZAPI Allocator pool_allocator_allocator(Pool_Allocator *state);

ZAPI void *pool_allocator_allocate(Pool_Allocator *allocator);
ZAPI void pool_allocator_free(Pool_Allocator *allocator, void *ptr);
ZAPI u32 pool_allocator_free_capacity(Pool_Allocator *pool);

}
