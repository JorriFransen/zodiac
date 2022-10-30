#include "allocator.h"

#include <zmemory.h>

namespace Zodiac
{

    static void *c_alloc_func(Allocator *allocator, Allocation_Mode mode, i64 size, void *old_ptr)
    {
        assert(allocator);
        assert(allocator == c_allocator() || allocator == err_allocator());

        switch (mode) {
            case Allocation_Mode::ALLOCATE: {
                assert(!old_ptr);
                return zallocate(size);
            }

            case Allocation_Mode::REALLOCATE: assert(false); break;

            case Allocation_Mode::FREE: {
                assert(old_ptr);
                zfree(old_ptr, size);
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
