#include "dynamic_allocator.h"

#include <platform/platform.h>

namespace Zodiac
{

struct Dynamic_Alloc_Header
{
    u32 size;
    u32 alignment; // TODO: This can be a u16, using a u32 for now as padding, since we are not passing alignment correctly (always 1)
};

Dynamic_Allocator_Block *allocate_block(u64 size);

void *dynamic_alloc_func(Allocator *allocator, Allocation_Mode mode, i64 size, void *old_ptr)
{

    auto das = (Dynamic_Allocator_State *)allocator->user_data;

    switch (mode) {
        case Allocation_Mode::ALLOCATE: {

            assert(size <= U32_MAX);

            u64 actual_size = size + sizeof(Dynamic_Alloc_Header);


            u64 offset;
            bool result = freelist_allocate_block(&das->current_block->freelist, actual_size, &offset);
            assert(result); // TODO: Add new blocks/try old blocks

            auto mem_block = (u8 *)das->current_block->memory;
            auto header = (Dynamic_Alloc_Header *)(mem_block + offset);
            header->size = size;
            header->alignment = 1;

            return mem_block + sizeof(Dynamic_Alloc_Header) + offset;
        }

        case Allocation_Mode::FREE: {
            assert(old_ptr);

            Dynamic_Allocator_Block *containing_block = nullptr;

            // Check the current block
            if (old_ptr >= das->current_block->memory && old_ptr < ((u8 *)das->current_block->memory) + das->current_block->freelist.total_size) {
                containing_block = das->current_block;
            } else {

                Dynamic_Allocator_Block *block = das->first_block;

                while (block) {

                    if (block != das->current_block) {

                        if (old_ptr >= block->memory && old_ptr < ((u8 *)block->memory) + block->freelist.total_size) {
                            containing_block = block;
                            break;
                        }

                    }

                    block = block->next;
                }
            }

            if (!containing_block) {
                zodiac_assert_fatal(!containing_block, "Dynamic allocator id not find block containing freed address...");
            }

            auto offset = ((u8 *)old_ptr) - ((u8 *)containing_block->memory) - sizeof(Dynamic_Alloc_Header);

            auto header = (Dynamic_Alloc_Header *)(((u8 *)containing_block->memory) + offset);

            bool result = freelist_free_block(&containing_block->freelist, header->size, offset);
            assert(result);
            return nullptr;
        }

        case Allocation_Mode::FREE_ALL: {
            assert(false && !"FREE_ALL not supported for dynamic_allocator");
            break;
        }

        case Allocation_Mode::REALLOCATE: {
            assert(false && !"REALLOCATE not supported for dynamic_allocator");
            break;
        }
    }

    assert(false);
    return nullptr;
}

bool dynamic_allocator_create(u64 initial_block_size, Dynamic_Allocator_State *out_allocator)
{
    assert(initial_block_size >= KIBIBYTE(4));
    assert(out_allocator);

    out_allocator->first_block = allocate_block(initial_block_size);
    out_allocator->current_block = out_allocator->first_block;
    out_allocator->initial_block_size = initial_block_size;

    return true;
}

Allocator dynamic_allocator_allocator(Dynamic_Allocator_State *dynamic_allocator)
{
    Allocator result;
    allocator_create(dynamic_alloc_func, dynamic_allocator, &result);
    return result;
}

Dynamic_Allocator_Block *allocate_block(u64 size)
{
    assert(size);

    u64 freelist_mem_req;
    freelist_create(size, &freelist_mem_req, nullptr, nullptr);

    assert(freelist_mem_req);

    u64 total_size = sizeof(Dynamic_Allocator_Block) + size + freelist_mem_req;
    assert(total_size);

    u8 *memory = (u8 *)platform_allocate(total_size);
    assert(memory);

    Dynamic_Allocator_Block *new_block = (Dynamic_Allocator_Block *)memory;
    u8 *freelist_mem = memory + sizeof(Dynamic_Allocator_Block);
    u8 *block_mem = freelist_mem + freelist_mem_req;

    freelist_create(size, &freelist_mem_req, &new_block->freelist, freelist_mem);
    new_block->memory = block_mem;
    new_block->next = nullptr;

    return new_block;
}

}
