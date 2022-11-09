#include "linear_allocator.h"

#include "zmemory.h"

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

void *linear_allocator_allocate(Linear_Allocator *allocator, u64 size)
{
    assert(allocator && allocator->memory && size);

    if (allocator->offset + size > allocator->size) {
        zodiac_error("Linear allocator out of space!");
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

}
