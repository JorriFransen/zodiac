#pragma once

#include <defines.h>

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
};

ZAPI void freelist_create(u64 total_size, u64 *memory_requirement, Freelist *out_list, void *memory);
ZAPI void freelist_destroy(Freelist *freelist);

ZAPI u64 freelist_free_space(Freelist *freelist);
ZAPI bool freelist_allocate_block(Freelist *freelist, u64 size, u64 *out_offset);
ZAPI bool freelist_free_block(Freelist *freelist, u64 size, u64 offset);

}
