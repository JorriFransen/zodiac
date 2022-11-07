#include "dynamic_allocator.h"

#include <memory/zmemory.h>
#include <platform/platform.h>

namespace Zodiac
{

struct Dynamic_Alloc_Header
{
    u8 *start;
    u32 size;
    u16 alignment;
};

Dynamic_Allocator_Block *allocate_block(u64 size);
void free_block(Dynamic_Allocator_Block *block);

void *dynamic_alloc_func(Allocator *allocator, Allocation_Mode mode, i64 size, void *old_ptr)
{

    auto das = (Dynamic_Allocator_State *)allocator->user_data;

    switch (mode) {
        case Allocation_Mode::ALLOCATE: {
            return dynamic_allocator_allocate(das, size);
        }

        case Allocation_Mode::FREE: {
            dynamic_allocator_free(das, old_ptr);
            return nullptr;
        }

        case Allocation_Mode::FREE_ALL: {
            assert(false && !"FREE_ALL not supported for dynamic_allocator");
            break;
        }

        case Allocation_Mode::REALLOCATE: {
            assert(false && !"REALLOCATE not supported for dynamic_allocator");
            break;
        }
    }

    assert(false);
    return nullptr;
}

bool dynamic_allocator_create(u64 initial_block_size, Dynamic_Allocator_State *out_allocator)
{
    assert(initial_block_size);
    assert(out_allocator);

    if (initial_block_size < KIBIBYTE(4)) {
        zodiac_warn("Creating dynamic allocator with small block size...");
    }

    out_allocator->first_block = allocate_block(initial_block_size);
    out_allocator->current_block = out_allocator->first_block;
    out_allocator->initial_block_size = initial_block_size;

    return true;
}

void dynamic_allocator_destroy(Dynamic_Allocator_State *state)
{
    assert(state && state->first_block);

    auto block = state->first_block;
    while (block) {

        free_block(block);

        auto next = block->next;

        zzeromem(state, sizeof(Dynamic_Allocator_State));

        block = next;
    }
}

Allocator dynamic_allocator_allocator(Dynamic_Allocator_State *state)
{
    Allocator result;
    allocator_create(dynamic_alloc_func, state, &result);
    return result;
}

Dynamic_Allocator_Block *allocate_block(u64 size)
{
    assert(size);

    u64 freelist_mem_req;
    freelist_create(size, &freelist_mem_req, nullptr, nullptr);

    assert(freelist_mem_req);

    u64 total_size = sizeof(Dynamic_Allocator_Block) + size + freelist_mem_req;
    assert(total_size);

    u8 *memory = (u8 *)platform_allocate(total_size);
    assert(memory);

    Dynamic_Allocator_Block *new_block = (Dynamic_Allocator_Block *)memory;
    u8 *freelist_mem = memory + sizeof(Dynamic_Allocator_Block);
    u8 *block_mem = freelist_mem + freelist_mem_req;

    freelist_create(size, &freelist_mem_req, &new_block->freelist, freelist_mem);
    new_block->memory = block_mem;
    new_block->next = nullptr;

    return new_block;
}

void free_block(Dynamic_Allocator_Block *block)
{
    assert(block);

    auto size = block->freelist.total_size;
    freelist_destroy(&block->freelist);
    platform_free(block, size);
}

void *dynamic_allocator_allocate(Dynamic_Allocator_State *state, u64 size)
{
    return dynamic_allocator_allocate_aligned(state, size, 1);
}

void *dynamic_allocator_allocate_aligned(Dynamic_Allocator_State *state, u64 size, u64 alignment)
{
    assert(state);
    assert(size <= U32_MAX);
    assert(is_power_of_two(alignment));

    u64 actual_size = size + alignment - 1 + sizeof(Dynamic_Alloc_Header);

    u64 offset;
    bool result = freelist_allocate_block(&state->current_block->freelist, actual_size, &offset);
    assert(result); // TODO: Add new blocks/try old blocks

    u8 *mem_block = ((u8 *)state->current_block->memory) + offset;
    u64 aligned_offset = get_aligned(((u64)mem_block) + sizeof(Dynamic_Alloc_Header), alignment);
    auto header = (Dynamic_Alloc_Header *)(aligned_offset - sizeof(Dynamic_Alloc_Header));
    header->start = mem_block;
    header->size = size;
    header->alignment = alignment;

    auto result_ptr = (void *)aligned_offset;
    assert((u64)result_ptr % alignment == 0);
    return result_ptr;
}

void dynamic_allocator_free(Dynamic_Allocator_State *state, void *memory)
{
    assert(state && memory);

    Dynamic_Allocator_Block *containing_block = nullptr;

    // Check the current block
    if (memory >= state->current_block->memory && memory < ((u8 *)state->current_block->memory) + state->current_block->freelist.total_size) {
        containing_block = state->current_block;
    } else {

        Dynamic_Allocator_Block *block = state->first_block;

        while (block) {

            if (block != state->current_block) {

                if (memory >= block->memory && memory < ((u8 *)block->memory) + block->freelist.total_size) {
                    containing_block = block;
                    break;
                }

            }

            block = block->next;
        }
    }

    if (!containing_block) {
        zodiac_assert_fatal(!containing_block, "Dynamic allocator id not find block containing freed address...");
    }

    auto header = (Dynamic_Alloc_Header *)(((u64)memory) - sizeof(Dynamic_Alloc_Header));
    u64 offset = header->start - ((u8 *)containing_block->memory);

    u64 total_size = header->size + header->alignment - 1 + sizeof(Dynamic_Alloc_Header);
    bool result = freelist_free_block(&containing_block->freelist, total_size, offset);
    assert(result);
}

u64 dynamic_allocator_free_space(Dynamic_Allocator_State *state)
{
    u64 total = 0;

    auto block = state->first_block;
    while (block) {

        total += freelist_free_space(&block->freelist);

        block = block->next;
    }

    return total;
}

ZAPI u64 dynamic_allocator_header_size()
{
    return sizeof(Dynamic_Alloc_Header);
}

}
