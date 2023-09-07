#pragma once

#include "common.h"
#include "defines.h"
#include "memory/allocator.h"
#include "util/asserts.h"

namespace Zodiac
{

template <typename Element_Type>
struct Stack
{
    Element_Type *buffer = nullptr;
    s64 sp = -1;
    s64 capacity = -1;

    Allocator *allocator = nullptr;
};

#define ZODIAC_STACK_DEFAULT_CAP 8

template <typename Element_Type>
void stack_init(Allocator *allocator, Stack<Element_Type> *stack, s64 initial_cap = ZODIAC_STACK_DEFAULT_CAP)
{
    assert(allocator);
    assert(stack);
    assert(initial_cap >= 0);

    if (initial_cap) {
        stack->buffer = alloc_array<Element_Type>(allocator, initial_cap);
        stack->sp = 0;
    } else {
        stack->buffer = nullptr;
    }

    stack->capacity = initial_cap;
    stack->allocator = allocator;
}

template <typename Element_Type>
void stack_free(Stack<Element_Type> *stack)
{
    if (stack->buffer) {
        assert(stack->capacity);
        free(stack->allocator, stack->buffer);
    }

    *stack = {};
}

template <typename Element_Type>
void stack_ensure_capacity(Stack<Element_Type> *stack)
{
    if (stack->sp >= stack->capacity || stack->sp == -1) {
        auto new_cap = max(stack->capacity * 2, (s64)1);
        auto new_buf = alloc_array<Element_Type>(stack->allocator, new_cap);
        memcpy(new_buf, stack->buffer, stack->capacity * sizeof(Element_Type));

        if (stack->buffer) {
            free(stack->allocator, stack->buffer);
        } else {
            stack->sp = 0;
        }

        stack->buffer = new_buf;
        stack->capacity = new_cap;
    }
}

template <typename Element_Type>
void stack_push(Stack<Element_Type> *stack, Element_Type element)
{
    stack_ensure_capacity(stack);

    stack->buffer[stack->sp] = element;
    stack->sp += 1;
}

template <typename Element_Type>
Element_Type stack_peek(Stack<Element_Type> *stack, s64 offset = 0)
{
    assert(stack);
    assert(stack->sp >= 1);

    assert(stack->sp > offset);

    return stack->buffer[(stack->sp - 1) - offset];
}

template <typename Element_Type>
Element_Type *stack_peek_ptr(Stack<Element_Type> *stack, s64 offset = 0)
{
    assert(stack);
    assert(stack->sp >= 1);
    assert(offset >= 0);

    assert(stack->sp > offset);

    return &stack->buffer[(stack->sp - 1) - offset];
}

template <typename Element_Type>
Element_Type stack_top(Stack<Element_Type> *stack)
{
    return stack_peek(stack);
}

template <typename Element_Type>
Element_Type *stack_top_ptr(Stack<Element_Type> *stack)
{
    return stack_peek_ptr(stack);
}

// Returns the element that is popped of first
template <typename Element_Type>
Element_Type stack_pop(Stack<Element_Type> *stack, s64 count)
{
    if (count == 0) return {};

    assert(count > 0);
    assert(stack->sp >= count);

    auto result = stack_peek(stack, count - 1);
    stack->sp -= count;

    return result;
}

template <typename Element_Type>
Element_Type stack_pop(Stack<Element_Type> *stack)
{
    assert(stack->sp >= 1);

    auto result = stack_peek(stack);
    stack->sp -= 1;

    return result;
}

template <typename Element_Type>
s64 stack_count(Stack<Element_Type> *stack)
{
    return stack->sp;
}

}
