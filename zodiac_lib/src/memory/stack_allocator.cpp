#include "stack_allocator.h"

#include <logger.h>
#include <memory/zmemory.h>

#include <asserts.h>

namespace Zodiac
{

file_local Stack_Block *create_block(u32 size);

void stack_allocator_create(u32 block_size, Stack_Allocator *out_allocator)
{
    assert(block_size && out_allocator);

    out_allocator->head = create_block(block_size);
    out_allocator->freelist = nullptr;
}

void stack_allocator_destroy(Stack_Allocator *allocator)
{
    assert(allocator && allocator->head);

    // The head block is on top, so walk down and free all the previous blocks
    auto used_block = allocator->head;
    while (used_block) {
        auto next = used_block->next;
        zfree(used_block);
        used_block = next;
    }

    // Free blocks from the freelist
    auto free_block = allocator->freelist;
    while (free_block) {
        auto next = free_block->next;
        zfree(free_block);
        free_block = next;
    }

    zzeromem(allocator, sizeof(Stack_Allocator));
}

Allocator stack_allocator_allocator(Stack_Allocator *state)
{
    assert(state);
    assert(false);
    return {};
}

void *stack_allocator_allocate(Stack_Allocator *state, u64 size)
{
    assert(state && size);

    return stack_allocator_allocate_aligned(state, size, 1);
}

void *stack_allocator_allocate_aligned(Stack_Allocator *state, u64 size, u64 alignment)
{
    assert(state && size && alignment);

    u64 actual_size = size + alignment - 1 + sizeof(Stack_Alloc_Header);

    if (actual_size > state->head->size - state->head->offset) {
        ZINFO("Growing stack allocator");
        assert_msg(false, "Stack allocator growing not implemented...");
    }

    u8 *mem_block = (u8 *)state->head->memory + state->head->offset;
    u64 aligned_offset = get_aligned((u64 )mem_block, alignment);

    auto result_ptr = (void *)aligned_offset;
    assert((u64)result_ptr % alignment == 0);

    auto header = (Stack_Alloc_Header *)((u8 *)aligned_offset + actual_size - sizeof(Stack_Alloc_Header));
    header->previous_offset = state->head->offset;

    state->head->offset += actual_size;

    return result_ptr;
}

void stack_allocator_free(Stack_Allocator *state, void *ptr)
{
    assert(state && ptr);
    assert(false);
}

u64 stack_allocator_free_space(Stack_Allocator *state)
{
    assert(state);
    assert(false);
    return 0;
}

file_local Stack_Block *create_block(u32 size)
{
    auto total_size = size + sizeof(Stack_Block);
    u8 *memory = (u8 *)zallocate(total_size);

    Stack_Block *result = (Stack_Block *)memory;
    result->memory = memory + sizeof(Stack_Block);
    result->offset = 0;
    result->size = size;

    return result;
}

}

