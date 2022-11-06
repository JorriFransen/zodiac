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
ZAPI Allocator dynamic_allocator_allocator(Dynamic_Allocator_State *dynamic_allocator);

}
