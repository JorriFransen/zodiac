#pragma once

#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "util/asserts.h"

namespace Zodiac
{

#define ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY 8

template <typename Element_Type>
struct Dynamic_Array
{
    Element_Type *data = nullptr;
    s64 count = 0;
    s64 capacity = 0;
    Allocator* backing_allocator = nullptr;

    Element_Type& operator[](s64 index) {
        assert(index >= 0 && index < count);
        return data[index];
    }

    const Element_Type& operator[](s64 index) const {
        assert(index >= 0 && index < count);
        return data[index];
    }
};

template <typename T>
struct Temp_Array;

template <typename Element_Type>
struct Array_Ref
{
    Element_Type *data = nullptr;
    s64 count = 0;

    Array_Ref() = default;

    Array_Ref(const Dynamic_Array<Element_Type> &dyn_arr) : data(dyn_arr.data), count(dyn_arr.count) { }
    Array_Ref(const Temp_Array<Element_Type> &temp_arr) : data(temp_arr.array.data), count(temp_arr.array.count) { }
    Array_Ref(Element_Type *data, s64 count) : data(data), count(count) { }

    template <size_t N>
    constexpr Array_Ref(const Element_Type (&c_arr)[N]) : data((Element_Type *)c_arr), count(N) {}

    Array_Ref(const Element_Type *begin, const Element_Type *end) : data(begin), count(end - begin) {}

    Element_Type& operator[](s64 index) {
        assert(index >= 0 && index < count);
        return data[index];
    }

    const Element_Type& operator[](s64 index) const {
        assert(index >= 0 && index < count);
        return data[index];
    }
};

template <typename Element_Type>
void dynamic_array_create(Allocator *backing_allocator, Dynamic_Array<Element_Type> *out_array, s64 capacity = ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY)
{
    assert(backing_allocator && out_array);
    assert(capacity >= 0);

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
    s64 new_cap = max(array->capacity * 2, (s64)1);
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
Element_Type *dynamic_array_insert(Dynamic_Array<Element_Type> *array, Element_Type element, s64 index = 0)
{
    assert(index >= 0 && index <= array->count);

    if (array->count >= array->capacity) {
        dynamic_array_grow(array);
    }

    auto copy_size = sizeof(Element_Type) * (array->count - index);
    zmemmove(&array->data[index + 1], &array->data[index], copy_size);

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
void dynamic_array_remove_unordered(Dynamic_Array<Element_Type> *array, s64 index)
{
    assert(array);
    assert(index >= 0);
    assert(array->count > index);

    array->data[index] = array->data[array->count - 1];
    array->count -= 1;
}

template <typename Element_Type>
void dynamic_array_remove_ordered(Dynamic_Array< Element_Type> *array, s64 index)
{
    assert(array && array->data && array->count);
    assert(index >= 0 && index <= array->count);

    if (index == array->count - 1) {
        array->count -= 1;
        return;
    }

    auto copy_count = array->count - 1 - index;
    auto copy_size = sizeof(Element_Type) * copy_count;

    zmemmove(&array->data[index], &array->data[index + 1], copy_size);

    array->count -= 1;
}

template <typename T>
struct Temp_Array
{
    Temporary_Allocator_Mark mark;
    Dynamic_Array<T> array;

    T& operator[](s64 index) {
        return array.operator[](index);
    }

    const T& operator[](s64 index) const {
        return array.operator[](index);
    }
};

template <typename T>
file_local Temp_Array<T> temp_array_create(Allocator *allocator, s64 cap = 0)
{
    Temp_Array<T> result;

    auto tas = (Temporary_Allocator *)allocator->user_data;
    assert(tas);

    result.mark = temporary_allocator_get_mark(tas);
    dynamic_array_create(allocator, &result.array, cap);
    return result;
}

template <typename T>
file_local void temp_array_destroy(Temp_Array<T> *ta)
{
    auto tas = (Temporary_Allocator *)ta->array.backing_allocator->user_data;
    assert(tas);

    temporary_allocator_reset(tas, ta->mark);
}

template <typename T>
file_local Dynamic_Array<T> temp_array_finalize(Allocator *allocator, Temp_Array<T> *ta)
{
    auto result = dynamic_array_copy(&ta->array, allocator);
    temp_array_destroy(ta);
    return result;
}

template <typename T>
void dynamic_array_append(Temp_Array<T> *ta, T element) {
    dynamic_array_append(&ta->array, element);
}

}
