#pragma once

#include "defines.h"
#include "linear_allocator.h"
#include "memory/allocator.h"

namespace Zodiac
{

struct Temporary_Allocator
{
    Linear_Allocator linear_allocator;
};

struct Temporary_Allocator_Mark
{
    u64 offset;
};

ZAPI Temporary_Allocator *temp_allocator();
ZAPI Allocator *temp_allocator_allocator();

ZAPI void deinitialize_global_temp_allocator();

ZAPI void temporary_allocator_create(u64 size, void *memory, Temporary_Allocator *out_allocator);
ZAPI void temporary_allocator_destroy(Temporary_Allocator *allocator);
ZAPI Allocator temporary_allocator_allocator(Temporary_Allocator *state);

ZAPI void *temporary_allocator_allocate(Temporary_Allocator *allocator, u64 size);
ZAPI Temporary_Allocator_Mark temporary_allocator_get_mark(Temporary_Allocator *allocator);
ZAPI void temporary_allocator_reset(Temporary_Allocator *allocator);
ZAPI void temporary_allocator_reset(Temporary_Allocator *allocator, Temporary_Allocator_Mark mark);

ZAPI u64 temporary_allocator_free_space(Temporary_Allocator *allocator);

}
