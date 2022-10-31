#pragma once

#include <defines.h>

#include "zmemory.h"

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
                                i64 size,
                                void *old_ptr);

struct Allocator
{
    Alloc_Function alloc_func = nullptr;
};

template <typename Element_Type>
Element_Type *alloc(Allocator *allocator)
{
    assert(allocator);
    i64 size = (i64)sizeof(Element_Type);
    Element_Type* result = (Element_Type *)allocator->alloc_func(allocator, Allocation_Mode::ALLOCATE, size, nullptr);
    zzeromem(result, size);
    return result;
}

template <typename Element_Type>
Element_Type *alloc_array(Allocator *allocator, i64 capacity)
{
    assert(allocator);
    assert(capacity > 0);
    i64 size = (i64)sizeof(Element_Type) * capacity;
    Element_Type* result = (Element_Type *)allocator->alloc_func(allocator, Allocation_Mode::ALLOCATE, size, nullptr);
    zzeromem(result, size);
    return result;
}

template <typename Element_Type>
void free(Allocator *allocator, Element_Type *pointer)
{
    assert(allocator);
    assert(pointer);
    allocator->alloc_func(allocator, Allocation_Mode::FREE, sizeof(Element_Type), (void*)pointer);
}

}
