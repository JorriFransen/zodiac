#include "linear_allocator.h"

#include "zmemory.h"

#include <logger.h>

namespace Zodiac
{

void linear_allocator_create(u64 size, void *memory, Linear_Allocator *out_allocator)
{
    assert(size && out_allocator);

    out_allocator->size = size;
    out_allocator->offset = 0;

    if (memory) {
        out_allocator->memory = memory;
        out_allocator->owns_memory = false;
    } else {
        out_allocator->memory = zallocate(size);
        assert(out_allocator->memory);
        out_allocator->owns_memory = true;
    }
}

void linear_allocator_destroy(Linear_Allocator *allocator)
{
    assert(allocator);

    if (allocator->owns_memory) {
        zfree(allocator->memory);
    }

    zzeromem(allocator, sizeof(Linear_Allocator));
}

void *linear_alloc_func(Allocator *allocator, Allocation_Mode mode, u64 size, u64 alignment, void *old_ptr)
{
    assert(allocator);

    auto las = (Linear_Allocator *)allocator->user_data;

    switch (mode) {
        case Allocation_Mode::ALLOCATE: {
            assert(alignment == 1);
            return linear_allocator_allocate(las, size);
        }

        case Allocation_Mode::FREE: {
            assert(false && !"FREE not supported for linear allocator");
            break;
        }

        case Allocation_Mode::FREE_ALL: {
            linear_allocator_free_all(las);
            return nullptr;
        }

        case Allocation_Mode::REALLOCATE: {
            assert(false && !"REALLOCATE not supported for linear allocator");
            break;
        }
    }

    assert(false);
    return nullptr;
}

Allocator linear_allocator_allocator(Linear_Allocator *state)
{
    assert(state);

    Allocator result;
    allocator_create(linear_alloc_func, state, &result);
    return result;
}

void *linear_allocator_allocate(Linear_Allocator *allocator, u64 size)
{
    assert(allocator && allocator->memory && size);

    if (allocator->offset + size > allocator->size) {
        ZERROR("Linear allocator out of space!");
        return nullptr;
    }

    auto result = ((u8 *)allocator->memory) + allocator->offset;
    allocator->offset += size;
    return (void *)result;
}

void linear_allocator_free_all(Linear_Allocator *allocator)
{
    assert(allocator && allocator->memory);

    allocator->offset = 0;
    zzeromem(allocator->memory, allocator->size);
}

u64 linear_allocator_free_space(Linear_Allocator *allocator)
{
    assert(allocator && allocator->memory);
    assert(allocator->offset <= allocator->size);

    return allocator->size - allocator->offset;
}

}
