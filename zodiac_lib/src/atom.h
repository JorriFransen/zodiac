#pragma once

#include <allocator.h>
#include <common.h>

namespace Zodiac
{

#define ATOM_TABLE_INITIAL_CAPACITY 64
#define ATOM_TABLE_INITIAL_BLOCK_SIZE 1024

    struct String_Ref;

    struct Atom
    {
        const char *data = nullptr;
        int64_t length = -1;
    };

    inline uint64_t hash_key(const Atom &atom)
    {
        return hash_c_string(atom.data, atom.length);
    }

    inline bool operator==(const Atom &lhs, const Atom &rhs)
    {
        if (lhs.length != rhs.length) return false;

        for (int64_t i = 0; i < lhs.length; i++) {
            if (lhs.data[i] != rhs.data[i]) return false;
        }

        return true;
    }

    inline bool operator !=(const Atom &lhs, const Atom &rhs)
    {
        return !operator==(lhs, rhs);
    }

    struct Atom_Block
    {
        char *first = nullptr;
        char *current = nullptr;
        char *end = nullptr;

        Atom_Block *next_block = nullptr;
    };

    struct Atom_Table
    {
        Allocator *allocator = nullptr;

        int64_t capacity = 0;

        Atom *atoms = nullptr;
        uint64_t *hashes = nullptr;

        Atom_Block first_block = {};
        Atom_Block *current_block = nullptr;
    };


    void atom_table_init(Allocator *allocator, Atom_Table *at, int64_t initial_capacity = ATOM_TABLE_INITIAL_CAPACITY);

    Atom atom_get(Atom_Table *at, const char *cstr, int64_t length);
    Atom atom_get(Atom_Table *at, const char *cstr);
    Atom atom_get(Atom_Table *at, const String_Ref &string_ref);

}