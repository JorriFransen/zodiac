#pragma once

#include "allocator.h"

#include <cassert>
#include <inttypes.h>

namespace Zodiac
{

    template <typename Element_Type>
    struct Queue
    {
         Element_Type *buffer = nullptr;
         int64_t front = -1;
         int64_t used = -1;
         int64_t capacity = 0;

         Allocator *allocator = nullptr;
    };


    template <typename Element_Type>
    void queue_init(Allocator *allocator, Queue<Element_Type> *queue, int64_t capacity = 8)
    {
        queue->buffer = alloc_array<Element_Type>(allocator, capacity);
        queue->front = -1;
        queue->used = 0;
        queue->capacity = capacity;

        queue->allocator = allocator;
    }

    template <typename Element_Type>
    void queue_enqueue(Queue<Element_Type> *queue, Element_Type value)
    {
        // assert(value);
        queue_ensure_capacity(queue);

        int64_t target_index = -1;
        if (queue->front == -1) {
            target_index = 0;
            queue->front = 0;
        } else {
            target_index = queue->front + queue->used;
            if (target_index >= queue->capacity) {
                target_index -= queue->capacity;
            }
        }

        assert(target_index >= 0);
        assert(target_index < queue->capacity);

        queue->buffer[target_index] = value;
        queue->used++;
    }

    template <typename Element_Type>
    void queue_ensure_capacity(Queue<Element_Type> *queue)
    {
        if (queue->used >= queue->capacity) {
            assert(queue->front >= 0);

            auto new_cap = queue->capacity * 2;
            assert(new_cap);

            Element_Type *new_buffer = alloc_array<Element_Type>(queue->allocator, new_cap);
            assert(new_buffer);

            // Copy front to end
            auto front_to_end_count = queue->capacity - queue->front;
            auto front_to_end_size = front_to_end_count * sizeof(Element_Type);
            memcpy(new_buffer, queue->buffer + queue->front, front_to_end_size);

            if (front_to_end_size < (queue->used * sizeof(Element_Type))) {
                // Copy remaining
                auto remaining_size = queue->used * sizeof(Element_Type) - front_to_end_size;
                memcpy(new_buffer + front_to_end_count, queue->buffer, remaining_size);
            }

            queue->front = 0;
            queue->capacity = new_cap;
            free(queue->allocator, queue->buffer);
            queue->buffer = new_buffer;
        }
    }

    template <typename Element_Type>
    int64_t queue_count(Queue<Element_Type> *queue)
    {
        return queue->used;
    }

    template <typename Element_Type>
    Element_Type queue_front(Queue<Element_Type> *queue)
    {
        assert(queue->capacity >= 1);
        assert(queue->used >= 1);

        return queue->buffer[queue->front];
    }

    template <typename Element_Type>
    Element_Type queue_dequeue(Queue<Element_Type> *queue)
    {
        auto front = queue_front(queue);

        queue->used--;
        queue->front++;

        if (queue->front >= queue->capacity)
            queue->front = 0;

        return front;
    }
}
