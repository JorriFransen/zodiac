#pragma once

#include <defines.h>

#include <containers/freelist.h>
#include <memory/allocator.h>

namespace Zodiac
{

struct Dynamic_Allocator_Block
{
    Freelist freelist;
    void *memory;

    Dynamic_Allocator_Block *next;
};

struct Dynamic_Allocator_State
{
    Dynamic_Allocator_Block *current_block;
    Dynamic_Allocator_Block *first_block;

    u64 initial_block_size;
};

ZAPI bool dynamic_allocator_create(u64 initial_block_size, Dynamic_Allocator_State *out_allocator);
ZAPI void dynamic_allocator_destroy(Dynamic_Allocator_State *state);
ZAPI Allocator dynamic_allocator_allocator(Dynamic_Allocator_State *state);

ZAPI void *dynamic_allocator_allocate(Dynamic_Allocator_State *state, u64 size);
ZAPI void *dynamic_allocator_allocate_aligned(Dynamic_Allocator_State *state, u64 size, u64 alignment);
ZAPI void dynamic_allocator_free(Dynamic_Allocator_State *state, void *memory);

ZAPI u64 dynamic_allocator_free_space(Dynamic_Allocator_State *state);
ZAPI u64 dynamic_allocator_header_size();


}
