#pragma once

#include <defines.h>

#include <memory/allocator.h>
#include <memory/zmemory.h>

namespace Zodiac
{

#define ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY 8

template <typename Element_Type>
struct Dynamic_Array
{
    Element_Type *data;
    u64 count;
    u64 capacity;
    Allocator* backing_allocator;

    Element_Type& operator[](u64 index) {
        assert(index >= 0 && index < count);
        return data[index];
    }

    const Element_Type& operator[](u64 index) const {
        assert(index >= 0 && index < count);
        return data[index];
    }
};

template <typename Element_Type>
void dynamic_array_create(Allocator *backing_allocator, Dynamic_Array<Element_Type> *out_array, u64 capacity = ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY)
{
    assert(backing_allocator && out_array);

    if (capacity) out_array->data = alloc_array<Element_Type>(backing_allocator, capacity);
    else out_array->data = nullptr;

    out_array->count = 0;
    out_array->capacity = capacity;
    out_array->backing_allocator = backing_allocator;
}

template <typename Element_Type>
void dynamic_array_free(Dynamic_Array<Element_Type> *array)
{
    if (array->data) {
        assert(array->capacity);
        free(array->backing_allocator, array->data);
    }

    zzeromem(array, sizeof(Dynamic_Array<Element_Type>));
}

template <typename Element_Type>
void dynamic_array_grow(Dynamic_Array<Element_Type> *array)
{
    u64 new_cap = array->capacity * 2;
    if (new_cap == 0) new_cap = ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY;
    assert(new_cap);

    Element_Type *new_data = alloc_array<Element_Type>(array->backing_allocator, new_cap);
    assert(new_data);
    if (array->capacity) {
        zmemcpy(new_data, array->data, sizeof(Element_Type) * array->count);
    }

    if (array->capacity) {
        assert(array->data);
        free(array->backing_allocator, array->data);
    }
    array->data = new_data;
    array->capacity = new_cap;
}

template <typename Element_Type>
void dynamic_array_append(Dynamic_Array<Element_Type> *array, Element_Type element)
{
    if (array->count >= array->capacity) {
        dynamic_array_grow(array);
        assert(array->capacity > array->count);
    }

    array->data[array->count] = element;
    array->count += 1;
}

}
