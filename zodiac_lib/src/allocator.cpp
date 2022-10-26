#include "allocator.h"

#include <cassert>
#include <stdlib.h>

namespace Zodiac
{

#ifdef WIN32
#pragma warning(push)
#pragma warning(disable:4100)
#endif

    void *c_alloc_func(Allocator *allocator, Allocation_Mode mode, int64_t size, void *old_ptr)
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

#ifdef WIN32
#pragma warning(pop)
#endif

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