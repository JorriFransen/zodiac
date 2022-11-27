#include "pool_allocator.h"

#include <memory/zmemory.h>
#include <logger.h>

namespace Zodiac
{

file_local Pool_Block *allocate_block(u32 element_size, u32 capacity);

void dynamic_pool_allocator_create(u32 block_capacity, u32 element_size, Pool_Allocator *out_allocator)
{
    assert(element_size && block_capacity);
    assert(element_size >= sizeof(void *) && "Dynamic_Pool_Allocator element size must be at least pointer size");

    out_allocator->element_size = element_size;
    out_allocator->block_capacity = block_capacity;

    out_allocator->first_block = allocate_block(element_size, block_capacity);
    out_allocator->current_block = out_allocator->first_block;

}

void dynamic_pool_allocator_destroy(Pool_Allocator *allocator)
{
    assert(allocator->element_size && allocator->block_capacity && allocator->first_block && allocator->current_block);

    auto block = allocator->first_block;
    while (block) {
        auto next_block = block->next;

        zfree(block);

        block = next_block;
    }

    zzeromem(allocator, sizeof(Pool_Allocator));
}

void *dynamic_pool_allocator_allocate(Pool_Allocator *allocator)
{
    Pool_Block *non_full_block = nullptr;

    if (allocator->current_block->head) {
        non_full_block = allocator->current_block;
    } else {
        auto block = allocator->first_block;
        while (block) {
            auto next_block = block->next;

            if (block->head) {
                non_full_block = block;
                break;
            }

            block = next_block;
        }
    }

    if (!non_full_block) {
        auto new_block = allocate_block(allocator->element_size, allocator->block_capacity);
        allocator->current_block->next = new_block;
        allocator->current_block = new_block;
        non_full_block = new_block;
    }

    assert(non_full_block);

    auto chunk = non_full_block->head;
    non_full_block->head = chunk->next;
    zzeromem(chunk, allocator->element_size);
    return chunk;
}

void dynamic_pool_allocator_free(Pool_Allocator *allocator, void *ptr)
{
    Pool_Block *containing_block = nullptr;

    auto block_size = allocator->element_size * allocator->block_capacity;
    if (ptr >= allocator->current_block->memory &&  ptr < (u8 *)allocator->current_block->memory + block_size) {
        containing_block = allocator->current_block;
    } else {

        auto block = allocator->first_block;
        while (block) {
            if (block == allocator->current_block) continue;

            if (ptr >= block->memory && ptr < (u8 *)block->memory + block_size) {
                containing_block = block;
                break;
            }

            block = block->next;
        }
    }

    if (!containing_block) {
        ZERROR("Invalid free (dynamic pool allocator)!");
        return;
    }

    Pool_Chunk *new_chunk = (Pool_Chunk *)ptr;
    new_chunk->next = containing_block->head;
    containing_block->head = new_chunk;
}

u32 dynamic_pool_allocator_free_capacity(Pool_Allocator *pool)
{
    // NOTE: We might want to cache this on the pool for speed
    u32 free_cap = 0;

    auto block = pool->first_block;
    while (block) {

        Pool_Chunk *chunk = block->head;
        while(chunk) {
            free_cap += 1;
            chunk = chunk->next;
        }

        block = block->next;
    }

    return free_cap;
}

file_local Pool_Block *allocate_block(u32 element_size, u32 capacity)
{
    auto block = (Pool_Block *)zallocate(sizeof(Pool_Block) + (element_size * capacity));
    block->next = nullptr;
    block->memory = ((u8 *)block) + sizeof(Pool_Block);
    block->head = (Pool_Chunk *)block->memory;

    // Setup the linked list of free chunks
    for (u64 i = 0; i < capacity; i++) {
        Pool_Chunk *chunk = (Pool_Chunk *)((u8 *)block->memory + (element_size * i));
        if (i < capacity - 1) {
            // Not the last node
            chunk->next = (Pool_Chunk *)((u8 *)block->memory + (element_size * (i + 1)));
        } else {
            chunk->next = nullptr;
        }
    }

    return block;
}

}
