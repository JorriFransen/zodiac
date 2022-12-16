#pragma once

#include <defines.h>

#include "allocator.h"
#include "containers/freelist.h"

namespace Zodiac
{

struct Dynamic_Allocator_Block
{
    Freelist freelist;
    void *memory;

    Dynamic_Allocator_Block *next;
};

struct Dynamic_Allocator
{
    Dynamic_Allocator_Block *current_block;
    Dynamic_Allocator_Block *first_block;

    u64 initial_block_size;
};

ZAPI bool dynamic_allocator_create(u64 initial_block_size, Dynamic_Allocator *out_allocator);
ZAPI void dynamic_allocator_destroy(Dynamic_Allocator *state);
ZAPI Allocator dynamic_allocator_allocator(Dynamic_Allocator *state);

ZAPI void *dynamic_allocator_allocate(Dynamic_Allocator *state, u64 size);
ZAPI void *dynamic_allocator_allocate_aligned(Dynamic_Allocator *state, u64 size, u64 alignment);
ZAPI void dynamic_allocator_free(Dynamic_Allocator *state, void *ptr);

ZAPI u64 dynamic_allocator_free_space(Dynamic_Allocator *state);
ZAPI u64 dynamic_allocator__header_size();


}
