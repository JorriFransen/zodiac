#include "atom.h"

#include <string.h>

#include "common.h"
#include "defines.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "util/zstring.h"

namespace Zodiac
{

static void atom_table_grow(Atom_Table *at);
static void atom_table_add_block(Atom_Table *at, Atom_Block *new_block, s64 block_size);
static Atom atom_table_add(Atom_Table *at, const char *cstr, s64 length);

void atom_table_init(Allocator *allocator, Atom_Table *at, s64 initial_capacity/*= ATOM_TABLE_INITIAL_CAPACITY*/)
{
    at->allocator = allocator;

    at->capacity = initial_capacity;

    at->atoms = alloc_array<Atom>(allocator, ATOM_TABLE_INITIAL_CAPACITY);
    at->hashes = alloc_array<u64>(allocator, ATOM_TABLE_INITIAL_CAPACITY);

    at->current_block = nullptr;
    atom_table_add_block(at, &at->first_block, ATOM_TABLE_INITIAL_BLOCK_SIZE);
}

void atom_table_init(Atom_Table *at, s64 initial_capacity /*= ATOM_TABLE_INITIAL_CAPACITY*/)
{
    atom_table_init(&dynamic_allocator, at, initial_capacity);
}

void atom_table_free(Atom_Table *at)
{
    free(at->allocator, at->atoms);
    free(at->allocator, at->hashes);

    auto block = &at->first_block;
    while (block) {

        auto next_block = block->next_block;

        free(at->allocator, block->first);
        if (block != &at->first_block) {
            free(at->allocator, block);
        }

        block = next_block;
    }
}

static void atom_table_grow(Atom_Table *at)
{
    auto new_cap = at->capacity * 2;

    auto old_cap = at->capacity;
    auto old_atoms = at->atoms;
    auto old_hashes = at->hashes;

    auto new_atoms = alloc_array<Atom>(at->allocator, new_cap);
    auto new_hashes = alloc_array<u64>(at->allocator, new_cap);

    at->capacity = new_cap;
    at->atoms = new_atoms;
    at->hashes = new_hashes;

    for (s64 i = 0; i < old_cap; i++) {

        u64 hash = old_hashes[i];
        u64 hash_index = hash % at->capacity;

        u64 iteration_count = 0;

        while (iteration_count < at->capacity) {

            if (at->hashes[hash_index] == 0) {

                at->hashes[hash_index] = hash;
                at->atoms[hash_index] = old_atoms[i];
                break;

            }  else {
                hash_index += 1;
                if (hash_index >= at->capacity) hash_index = 0;
            }

            iteration_count += 1;
        }
    }

    free(at->allocator, old_hashes);
    free(at->allocator, old_atoms);
}

static void atom_table_add_block(Atom_Table *at, Atom_Block *new_block, s64 block_size)
{
    assert(new_block->first == nullptr);
    assert(new_block->current == nullptr);
    assert(new_block->end == nullptr);

    new_block->first = alloc_array<char>(at->allocator, block_size);
    new_block->current = new_block->first;
    new_block->end = new_block->first + block_size;

    if (at->current_block) {
        at->current_block->next_block = new_block;
    }

    at->current_block = new_block;
}

static Atom atom_table_add(Atom_Table *at, const char *cstr, s64 length)
{
    assert(at->current_block);

    auto total_size = length + 1; // Include null terminator

    auto b = at->current_block;
    if (total_size > b->end - b->current) {

        // Add new block
        auto new_block_cap = b->end - b->first;
        while (new_block_cap < total_size) new_block_cap *= 2;

        b = alloc<Atom_Block>(at->allocator);
        *b = {};
        atom_table_add_block(at, b, new_block_cap);

    }

    char *dest = b->current;
    memcpy(dest, cstr, (size_t)length);
    dest[length] = '\0';

    Atom result = {
        .data = dest,
        .length = length,
    };

    b->current += total_size;

    return result;
}

Atom atom_get(Atom_Table *at, const char *cstr, s64 length)
{
    u64 hash = hash_string(cstr, length);
    u64 hash_index = hash % at->capacity;

    u64 iteration_count = 0;

    while (iteration_count < at->capacity) {

        if (at->hashes[hash_index] == hash &&
            string_equal(at->atoms[hash_index], String_Ref(cstr, length))) {

            return at->atoms[hash_index];

        } else if (at->hashes[hash_index] == 0) {

            Atom new_atom = atom_table_add(at, cstr, length);

            at->hashes[hash_index] = hash;
            at->atoms[hash_index] = new_atom;

            return new_atom;

        }

        hash_index += 1;
        if (hash_index >=  at->capacity) hash_index = 0;
        iteration_count += 1;
    }

    atom_table_grow(at);
    return atom_get(at, cstr, length);
}

Atom atom_get(Atom_Table *at, const char *cstr)
{
    auto length = strlen(cstr);
    return atom_get(at, cstr, length);
}

Atom atom_get(Atom_Table *at, const String_Ref &string_ref)
{
    return atom_get(at, string_ref.data, string_ref.length);
}

u64 hash_key(const Atom &atom)
{
    return hash_string(atom.data, atom.length);
}

}
