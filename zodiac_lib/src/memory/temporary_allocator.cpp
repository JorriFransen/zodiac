#include "temporary_allocator.h"

#include "memory/linear_allocator.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "zmemory.h"

namespace Zodiac
{

file_local void *temporary_alloc_func(Allocator *allocator, Allocation_Mode mode, u64 size, u64 alignment, void *old_ptr);

file_local bool temp_allocator_initialized = false;
file_local Temporary_Allocator temp_allocator_data = {};
file_local Allocator temp_allocator_allocator = { .alloc_func = temporary_alloc_func, .user_data = &temp_allocator_data };

Allocator *temp_allocator()
{
    if (!temp_allocator_initialized) {
        auto c_alloc = c_allocator();
        const auto size = KIBIBYTE(1);
        void *mem = alloc(c_alloc, size);

        temporary_allocator_create(size, mem, &temp_allocator_data);
        temp_allocator_initialized = true;
    }

    return &temp_allocator_allocator;
}

void temporary_allocator_create(u64 size, void *memory, Temporary_Allocator *out_allocator)
{
    assert(size && out_allocator);

    linear_allocator_create(size, memory, &out_allocator->linear_allocator);
}

void temporary_allocator_destroy(Temporary_Allocator *allocator)
{
    assert(allocator);

    linear_allocator_destroy(&allocator->linear_allocator);

    zzeromem(allocator, sizeof(Temporary_Allocator));
}

Allocator temporary_allocator_allocator(Temporary_Allocator *state)
{
    assert(state);

    Allocator result;
    allocator_create(&result, temporary_alloc_func, state, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC);
    return result;
}

void *temporary_allocator_allocate(Temporary_Allocator *allocator, u64 size)
{
    assert(allocator && size);

    if (temporary_allocator_free_space(allocator) < size) {
        ZERROR("Temporary allocator out of space!");
        return nullptr;
    }

    auto result = linear_allocator_allocate(&allocator->linear_allocator, size);
    assert(result);
    return result;
}

Temporary_Allocator_Mark temporary_allocator_get_mark(Temporary_Allocator *allocator)
{
    Temporary_Allocator_Mark result;
    result.offset = allocator->linear_allocator.offset;
    return result;
}

void temporary_allocator_reset(Temporary_Allocator *allocator)
{
    assert(allocator);

    linear_allocator_free_all(&allocator->linear_allocator);
}

void temporary_allocator_reset(Temporary_Allocator *allocator, Temporary_Allocator_Mark mark)
{
    assert(allocator);
    assert(mark.offset <= allocator->linear_allocator.offset);

    allocator->linear_allocator.offset = mark.offset;
}

u64 temporary_allocator_free_space(Temporary_Allocator *allocator)
{
    return linear_allocator_free_space(&allocator->linear_allocator);
}

file_local void *temporary_alloc_func(Allocator *allocator, Allocation_Mode mode, u64 size, u64 alignment, void *old_ptr)
{
    assert(allocator);

    auto tas = (Temporary_Allocator *)allocator->user_data;

    switch (mode) {
        case Allocation_Mode::ALLOCATE: {
            assert(alignment == 1);
            return temporary_allocator_allocate(tas, size);
        }

        case Allocation_Mode::FREE: {
            assert(false && !"FREE not supported for temporary allocator");
            break;
        }

        case Allocation_Mode::FREE_ALL: {
            temporary_allocator_reset(tas);
            return nullptr;
        }

        case Allocation_Mode::REALLOCATE: {
            assert(false && !"REALLOCATE not supported for temporary allocator");
            break;
        }
    }

    assert(false);
    return nullptr;
}

}
