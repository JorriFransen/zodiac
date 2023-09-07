#pragma once

#include "defines.h"

#include "allocator.h"

namespace Zodiac
{

struct Linear_Allocator
{
    u64 size;
    u64 offset;
    void *memory;
    bool owns_memory;
};

ZAPI void linear_allocator_create(u64 size, void *memory, Linear_Allocator *out_allocator);
ZAPI void linear_allocator_destroy(Linear_Allocator *allocator);
ZAPI Allocator linear_allocator_allocator(Linear_Allocator *state);

ZAPI void *linear_allocator_allocate(Linear_Allocator *allocator, u64 size);
ZAPI void linear_allocator_free_all(Linear_Allocator *allocator);

ZAPI u64 linear_allocator_free_space(Linear_Allocator *allocator);

}
