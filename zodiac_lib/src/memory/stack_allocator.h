#pragma once

#include <defines.h>

#include <memory/allocator.h>

namespace Zodiac
{

struct Stack_Alloc_Header
{
    u32 previous_offset;
};

struct Stack_Block
{
    void *memory;
    u32 offset;
    u32 size;

    Stack_Block *next; // Stores the next block in freelist, previous used blocks
};

struct Stack_Allocator
{
    Stack_Block *head;
    Stack_Block *freelist;
};

ZAPI void stack_allocator_create(u32 block_size, Stack_Allocator *out_allocator);
ZAPI void stack_allocator_destroy(Stack_Allocator *allocator);
ZAPI Allocator stack_allocator_allocator(Stack_Allocator *state);

ZAPI void *stack_allocator_allocate(Stack_Allocator *state, u64 size);
ZAPI void *stack_allocator_allocate_aligned(Stack_Allocator *state, u64 size, u64 alignment);
ZAPI void stack_allocator_free(Stack_Allocator *state, void *memory);

ZAPI u64 stack_allocator_free_space(Stack_Allocator *state);

}
