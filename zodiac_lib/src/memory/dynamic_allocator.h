#pragma once

#include <defines.h>

#include <memory/allocator.h>

namespace Zodiac
{

struct Dynamic_Allocator_State
{

};

ZAPI bool dynamic_allocator_create(u64 initial_block_size, Dynamic_Allocator_State *out_allocator);
ZAPI Allocator dynamic_allocator_allocator(Dynamic_Allocator_State *dynamic_allocator);

}
