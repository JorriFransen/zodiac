#include "allocator.h"

#include <zmemory.h>

namespace Zodiac
{

    void *c_alloc_func(Allocator *allocator, Allocation_Mode mode, i64 size, void *old_ptr)
    {
        assert(allocator);
        assert(allocator == c_allocator() || allocator == err_allocator());

        switch (mode) {
            case Allocation_Mode::ALLOCATE: {
                assert(!old_ptr);
                return kallocate(size);
            }

            case Allocation_Mode::REALLOCATE: assert(false); break;

            case Allocation_Mode::FREE: {
                assert(old_ptr);
                kfree(old_ptr, size);
                return nullptr;
            }
            case Allocation_Mode::FREE_ALL: assert(false); break;
        }

        assert(false);
        return nullptr;
    }

    static Allocator _c_allocator = { .alloc_func = c_alloc_func };

    Allocator *c_allocator()
    {
        return &_c_allocator;
    }

    static Allocator _err_allocator = { .alloc_func = c_alloc_func };

    Allocator *err_allocator()
    {
        return &_err_allocator;
    }

}