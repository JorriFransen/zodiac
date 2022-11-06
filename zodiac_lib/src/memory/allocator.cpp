#include "allocator.h"

#include "zmemory.h"

namespace Zodiac
{

void allocator_create(Alloc_Function alloc_func, void *user_data, Allocator *out_allocator)
{
    assert(alloc_func && out_allocator);

    out_allocator->alloc_func = alloc_func;
    out_allocator->user_data = user_data;
}

void *alloc(Allocator *allocator, u64 size)
{
    assert(allocator && size);
    return allocator->alloc_func(allocator, Allocation_Mode::ALLOCATE, size, nullptr);
}

void free(Allocator *allocator, void *memory)
{
    assert(allocator && memory);
    allocator->alloc_func(allocator, Allocation_Mode::FREE, 0, memory);
}

static void *c_alloc_func(Allocator *allocator, Allocation_Mode mode, i64 size, void *old_ptr)
{
    assert(allocator);
    assert(allocator == c_allocator() || allocator == err_allocator());

    switch (mode) {
        case Allocation_Mode::ALLOCATE: {
            assert(!old_ptr);
            return malloc(size);
        }

        case Allocation_Mode::REALLOCATE: assert(false); break;

        case Allocation_Mode::FREE: {
            assert(old_ptr);
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
