#pragma once

#include "asserts.h"
#include "defines.h"
#include "memory/allocator.h"

namespace Zodiac
{

#define ZODIAC_QUEUE_DEFAULT_CAPACITY 8

template <typename Element_Type>
struct Queue
{
    Element_Type *data;
    s64 front;
    s64 back;
    s64 capacity;
    Allocator *allocator;
};

template <typename Element_Type>
void queue_create(Allocator *backing_allocator, Queue<Element_Type> *out_queue, u64 capacity = ZODIAC_QUEUE_DEFAULT_CAPACITY)
{
    assert(backing_allocator && out_queue);

    if (capacity) {
        out_queue->data = alloc_array<Element_Type>(backing_allocator, capacity);
    }

    out_queue->front = -1;
    out_queue->back = -1;
    out_queue->capacity = capacity;
    out_queue->allocator = backing_allocator;
}

template <typename Element_Type>
s64 queue_empty(Queue<Element_Type> *queue)
{
    return queue->front == -1;
}

template <typename Element_Type>
s64 queue_used(Queue<Element_Type> *queue)
{
    if (queue->front == queue->back) return 1;
    else if (queue->front < queue->back) {
        assert(queue->front >= 0 && queue->back > 0);
        return (queue->back + 1) - queue->front;
    } else {
        assert(queue->front >= 0 && queue->back > 0);
        return queue->capacity - (queue->front - (queue->back + 1));
    }
}

template <typename Element_Type>
void queue_enqueue(Queue<Element_Type> *queue, Element_Type element)
{
    if (queue_used(queue) == queue->capacity) {
        assert_msg(false, "TODO: Grow queue");
    }

    if (queue->front == -1) {
        queue->front = 0;
        queue->back = 0;
    } else if (queue->back == (queue->capacity - 1)){
        assert(queue->front > 0);
        queue->back = 0;
    } else {
        queue->back += 1;
    }

    queue->data[queue->back] = element;
}

template <typename Element_Type>
Element_Type queue_dequeue(Queue<Element_Type> *queue)
{
    if (queue->front == -1) {
        assert_msg(false, "Cannot dequeue from empty queue");
    }

    Element_Type result = queue->data[queue->front];
    if (queue->front == queue->capacity - 1) {
        queue->front = 0;
    } else if (queue->front == queue->back) {
        queue->front = -1;
        queue->back = -1;
    } else {
        queue->front += 1;
    }

    return result;
}

template <typename Element_Type>
Element_Type queue_peek(Queue<Element_Type> *queue, u64 offset = 0)
{
    if (queue->front == -1) {
        assert_msg(false, "Cannot peek empty queue");
    }

    assert(offset < queue_used(queue));

    u64 index = (queue->front + offset) % (queue->capacity - 1);
    return queue->data[index];
}

template <typename Element_Type>
Element_Type *queue_peek_ptr(Queue<Element_Type> *queue, u64 offset = 0)
{
    if (queue->front == -1) {
        assert_msg(false, "Cannot peek empty queue");
    }

    assert(offset < queue_used(queue));

    u64 index = (queue->front + offset) % (queue->capacity - 1);
    return &queue->data[index];
}

}
