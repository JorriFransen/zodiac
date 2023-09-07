#pragma once

#include "defines.h"

namespace Zodiac
{

struct Allocator;

ZAPI Allocator *c_allocator();
ZAPI Allocator *err_allocator();

enum class Allocation_Mode
{
    ALLOCATE,
    REALLOCATE,
    FREE,
    FREE_ALL,
};

typedef void *(*Alloc_Function)(Allocator *allocator,
                                Allocation_Mode mode,
                                u64 size,
                                u64 alignment,
                                void *old_ptr);

typedef u64 Allocator_Flags;

enum Allocator_Flag : Allocator_Flags
{
    ALLOCATOR_FLAG_NONE         = 0x00,
    ALLOCATOR_FLAG_CANT_FREE    = 0x01,
    ALLOCATOR_FLAG_CANT_REALLOC = 0x02,
};

struct Allocator
{
    Allocator_Flags flags;

    Alloc_Function alloc_func = nullptr;
    void *user_data = nullptr;
};

ZAPI void allocator_create(Allocator *out_allocator, Alloc_Function alloc_func, void *user_data, Allocator_Flags flags = ALLOCATOR_FLAG_NONE);

ZAPI void *alloc(Allocator *allocator, u64 size);
ZAPI void *alloc_aligned(Allocator *allocator, u64 size, u64 alignment);
ZAPI void free(Allocator *allocator, void *memory);
ZAPI void free(Allocator *allocator, const void *memory);
ZAPI void free_all(Allocator *allocator);

template <typename Element_Type>
Element_Type *alloc(Allocator *allocator)
{
    return (Element_Type *)alloc_aligned(allocator, sizeof(Element_Type), 1);
}

template <typename Element_Type>
Element_Type *alloc_aligned(Allocator *allocator, u64 alignment)
{
    return (Element_Type *)alloc_aligned(allocator, sizeof(Element_Type), alignment);
}

template <typename Element_Type>
Element_Type *alloc_array(Allocator *allocator, u64 capacity)
{
    return (Element_Type *)alloc_aligned(allocator, sizeof(Element_Type) * capacity, 1);
}

template <typename Element_Type>
Element_Type *alloc_array(Allocator *allocator, u64 capacity, u64 alignment)
{
    return (Element_Type *)alloc_aligned(allocator, sizeof(Element_Type) * capacity, alignment);
}

}
