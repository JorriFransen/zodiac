#pragma once

#include <defines.h>

namespace Zodiac
{

struct Dynamic_Pool_Chunk
{
    Dynamic_Pool_Chunk* next;
};

struct Dynamic_Pool_Block
{
    Dynamic_Pool_Block *next;
    void *memory;
    Dynamic_Pool_Chunk *head;
};

struct Dynamic_Pool_Allocator
{
    u32 element_size;
    u32 block_capacity;

    Dynamic_Pool_Block *first_block;
    Dynamic_Pool_Block *current_block;
};

ZAPI void dynamic_pool_allocator_create(u32 block_capacity, u32 element_size, Dynamic_Pool_Allocator *out_allocator);
ZAPI void dynamic_pool_allocator_destroy(Dynamic_Pool_Allocator *allocator);

ZAPI void *dynamic_pool_allocator_allocate(Dynamic_Pool_Allocator *allocator);
ZAPI void dynamic_pool_allocator_free(Dynamic_Pool_Allocator *allocator, void *ptr);
ZAPI u32 dynamic_pool_allocator_free_capacity(Dynamic_Pool_Allocator *pool);

}
