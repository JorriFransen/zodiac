#include "freelist.h"

namespace Zodiac
{

void reset_node(Freelist *freelist, Freelist_Node *node);
Freelist_Node *freelist_get_node(Freelist *freelist);

void freelist_create(u64 total_size, u64 *memory_requirement, Freelist *out_list, void *memory)
{
    assert(total_size && memory_requirement && out_list);

    auto max_entries = total_size / sizeof(void *);
    zodiac_assert_fatal(max_entries >= 20, "Creating freelist for small memory block failed...");

    *memory_requirement = max_entries * sizeof(Freelist_Node);
    if (!memory) return;

    out_list->total_size = total_size;
    out_list->max_entries = max_entries;

    out_list->nodes = (Freelist_Node *)memory;
    out_list->head = &out_list->nodes[0];

    out_list->head->offset = 0;
    out_list->head->size = total_size;
    out_list->head->next = nullptr;

    for (i64 i = 1; i < out_list->max_entries; i++) {
        out_list->nodes[i].offset = ZODIAC_FREELIST_INVALID_OFFSET;
        out_list->nodes[i].size = ZODIAC_FREELIST_INVALID_SIZE;
    }
}

void freelist_destroy(Freelist *freelist)
{
    assert(freelist);

    zzeromem(freelist->nodes, sizeof(Freelist_Node) * freelist->max_entries);
    zzeromem(freelist, sizeof(Freelist));
}

u64 freelist_free_space(Freelist *freelist)
{
    assert(freelist);

    u64 total = 0;
    auto node = freelist->head;
    while (node) {
        total += node->size;
        node = node->next;
    }

    return total;
}

bool freelist_allocate_block(Freelist *freelist, u64 size, u64 *out_offset)
{
    assert(freelist && size && out_offset);

    Freelist_Node *node = freelist->head;
    Freelist_Node *previous_node = nullptr;

    while (node) {
        if (node->size == size) {

            // Exact match
            *out_offset = node->offset;

            if (previous_node) {
                previous_node->next = node->next;
            } else {
                freelist->head = node->next;
            }

            reset_node(freelist, node);
            return true;

        } else if (node->size > size) {
            *out_offset = node->offset;
            node->size -= size;
            node->offset += size;
            return true;
        }

        previous_node = node;
        node = node->next;
    }

    zodiac_warn("Freelist out of space...");
    return false;
}

bool freelist_free_block(Freelist *freelist, u64 size, u64 offset)
{
    assert(freelist && freelist->nodes && size);

    Freelist_Node *node = freelist->head;
    Freelist_Node *previous_node = nullptr;

    if (!node) {
        // Everything is allocated, make one new node for all the memory

        assert(offset == 0);
        assert(size == freelist->total_size);

        Freelist_Node *new_node = freelist_get_node(freelist);
        new_node->offset = 0;
        new_node->size = 0;
        new_node->next = nullptr;
        freelist->head = new_node;
        return true;
    }

    while (node) {

        if (node->offset > offset) {

            Freelist_Node *new_node = freelist_get_node(freelist);
            new_node->offset = offset;
            new_node->size = size;

            if (previous_node) {
                previous_node->next = new_node;
                new_node->next = node;
            } else {
                new_node->next = node;
                freelist->head = new_node;
            }

            // Optionally join new node with next node
            if (new_node->next && new_node->offset + new_node->size == new_node->next->offset) {
                new_node->size += new_node->next->size;
                auto removed_node = new_node->next;
                new_node->next = removed_node->next;

                reset_node(freelist, removed_node);
            }

            // Optionally join new node with previous node
            if (previous_node && previous_node->offset + previous_node->size == new_node->offset) {
                previous_node->size += new_node->size;
                auto removed_node = new_node;
                previous_node->next = removed_node->next;

                reset_node(freelist, removed_node);
            }

            return true;

        }

        previous_node = node;
        node = node->next;
    }

    zodiac_assert_fatal(false, "Freelist unable to find block to be freed...");
    return false;
}

void reset_node(Freelist *freelist, Freelist_Node *node)
{
    assert(freelist && node);

    node->offset = ZODIAC_FREELIST_INVALID_OFFSET;
    node->size = ZODIAC_FREELIST_INVALID_SIZE;
    node->next = nullptr;
}

Freelist_Node *freelist_get_node(Freelist *freelist)
{
    for (u64 i = 0; i < freelist->max_entries; i++) {
        if (freelist->nodes[i].offset == ZODIAC_FREELIST_INVALID_OFFSET) {
            return &freelist->nodes[i];
        }
    }

    return nullptr;
}

}
