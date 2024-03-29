#include "allocator.h"

#include <stdlib.h>

#include "common.h"
#include "util/asserts.h"
#include "zmemory.h"

namespace Zodiac
{

void allocator_create(Allocator *out_allocator, Alloc_Function alloc_func, void *user_data, Allocator_Flags flags/*=ALLOCATOR_FLAG_NONE*/)
{
    assert(alloc_func && out_allocator);

    out_allocator->flags = flags;
    out_allocator->alloc_func = alloc_func;
    out_allocator->user_data = user_data;
}

void *alloc(Allocator *allocator, u64 size)
{
    return alloc_aligned(allocator, size, 1);
}

void *alloc_aligned(Allocator *allocator, u64 size, u64 alignment)
{
    debug_assert(allocator && allocator->alloc_func && size && alignment);
    debug_assert(is_power_of_two(alignment));
    void *result = allocator->alloc_func(allocator, Allocation_Mode::ALLOCATE, size, alignment, nullptr);
    zzeromem(result, size);
    return result;
}

void free(Allocator *allocator, void *memory)
{
    assert(allocator && memory);
    if (!(allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
        allocator->alloc_func(allocator, Allocation_Mode::FREE, 0, 0, memory);
    }
}

void free(Allocator *allocator, const void *memory)
{
    free(allocator, (void *)memory);
}

void free_all(Allocator *allocator)
{
    assert(allocator);
    allocator->alloc_func(allocator, Allocation_Mode::FREE_ALL, 0, 0, nullptr);
}

static void *c_alloc_func(Allocator *allocator, Allocation_Mode mode, u64 size, u64 alignment, void *old_ptr)
{
    assert(allocator);
    assert(allocator == c_allocator() || allocator == err_allocator());

    switch (mode) {
        case Allocation_Mode::ALLOCATE: {
            assert(!old_ptr);
            assert(alignment == 1);
            return ::malloc(size);
        }

        case Allocation_Mode::REALLOCATE: assert(false); break;

        case Allocation_Mode::FREE: {
            assert(old_ptr);
            assert(alignment == 0);
            ::free(old_ptr);
            return nullptr;
        }
        case Allocation_Mode::FREE_ALL: assert(false); break;
    }

    assert(false);
    return nullptr;
}

static Allocator c_allocator_data = { .alloc_func = Zodiac::c_alloc_func };

Allocator *c_allocator()
{
    return &c_allocator_data;
}

static Allocator err_allocator_data = { .alloc_func = Zodiac::c_alloc_func };

Allocator *err_allocator()
{
    return &err_allocator_data;
}
}
