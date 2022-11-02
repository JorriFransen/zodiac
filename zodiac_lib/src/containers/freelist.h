#pragma once

#include <defines.h>

#include <memory/allocator.h>

namespace Zodiac
{

#define ZODIAC_FREELIST_INVALID_OFFSET U64_MAX
#define ZODIAC_FREELIST_INVALID_SIZE U64_MAX

struct Freelist_Node
{
    u64 offset;
    u64 size;
    Freelist_Node *next;
};

struct Freelist
{
    u64 total_size;
    u64 max_entries;

    Freelist_Node *head;
    Freelist_Node *nodes;

    Allocator *backing_allocator; // Used for node allocation
};

ZAPI void freelist_create(Allocator *backing_allocator, u64 total_size, Freelist *out_list);
ZAPI void freelist_free(Freelist *freelist);

ZAPI u64 freelist_free_space(Freelist *freelist);
ZAPI bool freelist_allocate_block(Freelist *freelist, u64 size, u64 *out_offset);
ZAPI bool freelist_free_block(Freelist *freelist, u64 size, u64 offset);

}