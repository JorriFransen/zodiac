#pragma once

#include <cassert>
#include <inttypes.h>

namespace Zodiac
{

struct Allocator;

enum class Allocation_Mode
{
    ALLOCATE,
    REALLOCATE,
    FREE,
    FREE_ALL,
};

typedef void *(*Alloc_Function)(Allocator *allocator,
                                Allocation_Mode mode,
                                int64_t size,
                                void *old_ptr);

struct Allocator
{
    Alloc_Function alloc_func = nullptr;
};

Allocator *c_allocator();
Allocator *err_allocator();

template <typename T>
T *alloc(Allocator *allocator)
{
    assert(allocator);
    auto size = sizeof(T);
    auto result = (T *)allocator->alloc_func(allocator, Allocation_Mode::ALLOCATE, size, nullptr);
    return result;
}

template <typename Element_Type>
Element_Type *alloc_array(Allocator *allocator, int64_t capacity)
{
    assert(allocator);
    assert(capacity > 0);
    auto size = sizeof(Element_Type) * capacity;
    auto result = (Element_Type *)allocator->alloc_func(allocator, Allocation_Mode::ALLOCATE, size, nullptr);
    return result;
}

template <typename Element_Type>
void free(Allocator *allocator, Element_Type *pointer)
{
    assert(allocator);
    assert(pointer);
    allocator->alloc_func(allocator, Allocation_Mode::FREE, 0, (void*)pointer);
}

}