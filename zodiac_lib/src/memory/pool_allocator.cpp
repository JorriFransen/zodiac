#include "pool_allocator.h"

#include "zmemory.h"

#include <logger.h>

namespace Zodiac
{

void pool_allocator_create_freelist(Pool_Allocator *allocator);

void pool_allocator_create(u32 capacity, u32 element_size, void *memory, Pool_Allocator *out_allocator)
{
    assert(capacity && element_size && out_allocator);
    assert(element_size >= sizeof(void *) && "Pool_Allocator element size must be at least pointer size");

    out_allocator->capacity = capacity;
    out_allocator->element_size = element_size;

    if (memory) {
        out_allocator->memory = memory;
        out_allocator->owns_memory = false;
    } else {
        out_allocator->memory = zallocate(element_size * capacity);
        out_allocator->owns_memory = true;
    }

    pool_allocator_create_freelist(out_allocator);
}

void pool_allocator_destroy(Pool_Allocator *allocator)
{
    assert(allocator && allocator->memory);

    zzeromem(allocator->memory, allocator->capacity * allocator->element_size);

    if (allocator->owns_memory) {
        zfree(allocator->memory);
    }

    zzeromem(allocator, sizeof(Pool_Allocator));
}

void *pool_allocator_allocate(Pool_Allocator *allocator)
{
    assert(allocator && allocator->memory);

    if (allocator->head) {
        auto chunk = allocator->head;
        allocator->head = chunk->next;
        zzeromem(chunk, allocator->element_size);
        return chunk;
    } else {
        ZERROR("Pool allocator out of space!");
        return nullptr;
    }
}

void pool_allocator_free(Pool_Allocator *allocator, void *memory)
{
    assert(allocator && memory);
    assert(memory >= allocator->memory && memory < ((u8 *)allocator->memory) + (allocator->capacity * allocator->element_size));

    Pool_Chunk *new_chunk = (Pool_Chunk *)memory;
    new_chunk->next = allocator->head;
    allocator->head = new_chunk;
}

void pool_allocator_create_freelist(Pool_Allocator *allocator)
{
    assert(allocator && allocator->memory);

    allocator->head = (Pool_Chunk *)allocator->memory;

    // Setup the linked list of free chunks
    for (u64 i = 0; i < allocator->capacity; i++) {

        Pool_Chunk *chunk = (Pool_Chunk *)((u8 *)allocator->memory + (allocator->element_size * i));

        if (i < allocator->capacity - 1) {
            // Not the last node
            chunk->next = (Pool_Chunk *)((u8 *)allocator->memory + (allocator->element_size * (i + 1)));
        } else {
            // Last node
            chunk->next = nullptr;
        }
    }
}

u32 pool_allocator_free_capacity(Pool_Allocator *pool)
{
    // NOTE: We might want to cache this on the pool for speed
    u32 free_cap = 0;

    Pool_Chunk *chunk = pool->head;
    while (chunk) {
        free_cap += 1;
        chunk = chunk ->next;
    }

    return free_cap;
}

}
