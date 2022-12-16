#pragma once

#include "allocator.h"
#include "defines.h"

namespace Zodiac
{

struct Stack_Block
{
    void *memory;
    u32 offset;
    u32 size;

    Stack_Block *next;
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
ZAPI bool stack_allocator_free(Stack_Allocator *state, void *memory);
ZAPI void stack_allocator_free_all(Stack_Allocator *state);

ZAPI u64 stack_allocator_free_space(Stack_Allocator *state);
ZAPI u64 stack_allocator__header_size();

}
