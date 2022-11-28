#include "stack_allocator.h"

#include <asserts.h>
#include <common.h>
#include <logger.h>
#include <memory/zmemory.h>

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

        // Iterate the freelist to find a block with enough space
        auto free_block = state->freelist;
        Stack_Block *previous = nullptr;
        while (free_block) {
            auto next = free_block->next;

            if (free_block->size >= actual_size) {
                if (previous) {
                    previous->next = next;
                } else {
                    state->freelist = next;
                }
                free_block->next = nullptr;
                break;
            }

            previous = free_block;
            free_block = next;
        }

        if (free_block) {
            free_block->next = state->head;
            state->head = free_block;
        } else {
            auto new_block_size = max(size + sizeof(Stack_Alloc_Header), (u64)state->head->size);
            auto new_block = create_block(new_block_size);
            new_block->next = state->head;
            state->head = new_block;
        }
    }

    assert(actual_size <= state->head->size - state->head->offset);

    u8 *base_ptr = (u8 *)state->head->memory + state->head->offset;
    u64 aligned_offset = get_aligned((u64)base_ptr, alignment);
    auto result_ptr = (void *)aligned_offset;

    auto header = (Stack_Alloc_Header *)((u8 *)result_ptr + size);
    header->ptr = result_ptr;
    header->previous_offset = state->head->offset;

    state->head->offset += actual_size;

    assert((u64)result_ptr % alignment == 0);
    return result_ptr;
}

bool stack_allocator_free(Stack_Allocator *state, void *ptr)
{
    assert(state && ptr);

    if (state->head->offset == 0 && state->head->next != nullptr) {
        auto old_block = state->head;
        state->head = old_block->next;

        old_block->next = state->freelist;
        state->freelist = old_block;
    }

    assert(state->head->offset > sizeof(Stack_Alloc_Header));
    if (ptr < state->head->memory || ptr >= (u8 *)state->head->memory + state->head->size) {
        ZERROR("Stack allocator attempting to free memory which is not in the head block...");
        return false;
    }

    auto header = (Stack_Alloc_Header *)((u8 *)state->head->memory + state->head->offset - sizeof(Stack_Alloc_Header));

    if (header->ptr != ptr) {
        ZERROR("Stack allocator attempting to free memory which is not on top of the stack...");
        return false;
    }

    state->head->offset = header->previous_offset;

    return true;
}

u64 stack_allocator_free_space(Stack_Allocator *state)
{
    u64 total_free = 0;

    auto used_block = state->head;
    while (used_block) {
        auto next = used_block->next;
        total_free += (used_block->size - used_block->offset);
        used_block = next;
    }

    auto free_block = state->freelist;
    while (free_block) {
        auto next = free_block->next;
        total_free += free_block->size;
        free_block = next;
    }

    return total_free;
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

ZAPI u64 stack_allocator__header_size()
{
    return sizeof(Stack_Alloc_Header);
}

}

