#include "dynamic_allocator.h"

#include <platform/platform.h>

namespace Zodiac
{

void *dynamic_alloc_func(Allocator *allocator, Allocation_Mode mode, i64 size, void *old_ptr)
{
    switch (mode) {
        case Allocation_Mode::ALLOCATE: {
            return platform_allocate(size);
        }

        case Allocation_Mode::FREE: {
            platform_free(old_ptr, size);
            return old_ptr;
        }

        case Allocation_Mode::FREE_ALL: {
            assert(false && !"FREE_ALL not supported for dynamic_allocator");
            break;
        }

        case Allocation_Mode::REALLOCATE: {
            assert(false && !"REALLOCATE not supported for dynamic_allocator");
            break;
        }
    }

    assert(false);
    return nullptr;
}

bool dynamic_allocator_create(u64 initial_block_size, Dynamic_Allocator_State *out_allocator)
{
    assert(false);
    return false;
}

Allocator dynamic_allocator_allocator(Dynamic_Allocator_State *dynamic_allocator)
{
    assert(false);
    return {};
}

}
