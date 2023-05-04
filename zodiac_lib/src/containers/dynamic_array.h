#pragma once

#include "memory/zmemory.h"

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
struct Array_Ref
{
    Element_Type *data = nullptr;
    u64 count = 0;

    Array_Ref() = default;

    Array_Ref(const Dynamic_Array<Element_Type> &dyn_arr) : data(dyn_arr.data), count(dyn_arr.count) { }
    Array_Ref(Element_Type *data, u64 count) : data(data), count(count) { }

    template <size_t N>
    constexpr Array_Ref(const Element_Type (&c_arr)[N]) : data((Element_Type *)c_arr), count(N) {}

    Array_Ref(const Element_Type *begin, const Element_Type *end) : data(begin), count(end - begin) {}

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
        if (!(array->backing_allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
            free(array->backing_allocator, array->data);
        }
    }

    zzeromem(array, sizeof(Dynamic_Array<Element_Type>));
}

template <typename Element_Type>
void dynamic_array_grow(Dynamic_Array<Element_Type> *array)
{
    u64 new_cap = max(array->capacity * 2, (u64)1);
    assert(new_cap);

    Element_Type *new_data = alloc_array<Element_Type>(array->backing_allocator, new_cap);
    assert(new_data);
    if (array->capacity) {
        zmemcpy(new_data, array->data, sizeof(Element_Type) * array->count);
    }

    if (array->capacity) {
        assert(array->data);
        if (!(array->backing_allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
            free(array->backing_allocator, array->data);
        }
    }
    array->data = new_data;
    array->capacity = new_cap;
}

template <typename Element_Type>
Element_Type *dynamic_array_append(Dynamic_Array<Element_Type> *array, Element_Type element)
{
    if (array->count >= array->capacity) {
        dynamic_array_grow(array);
        assert(array->capacity > array->count);
    }

    auto index = array->count;

    array->data[index] = element;
    array->count += 1;

    return &array->data[index];
}

template <typename Element_Type>
Dynamic_Array<Element_Type> dynamic_array_copy(const Array_Ref<Element_Type> &source, Allocator *allocator)
{
    if (source.count == 0) return {};

    Dynamic_Array<Element_Type> result;
    dynamic_array_create(allocator, &result, source.count);

    zmemcpy(result.data, source.data, sizeof(Element_Type) * source.count);
    result.count = source.count;

    return result;
}

template <typename Element_Type>
Dynamic_Array<Element_Type> dynamic_array_copy(Dynamic_Array<Element_Type> *source, Allocator *allocator)
{
    return dynamic_array_copy(Array_Ref<Element_Type>(*source), allocator);
}

template <typename Element_Type>
void dynamic_array_remove_unordered(Dynamic_Array<Element_Type> *array, u64 index)
{
    assert(array);
    assert(array->count > index);

    array->data[index] = array->data[array->count - 1];
    array->count -= 1;
}

}
