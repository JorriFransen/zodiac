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
        i64 length = -1;
    };

    inline bool operator==(const Atom &lhs, const Atom &rhs)
    {
        if (lhs.length != rhs.length) return false;

        for (i64 i = 0; i < lhs.length; i++) {
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

        i64 capacity = 0;

        Atom *atoms = nullptr;
        u64 *hashes = nullptr;

        Atom_Block first_block = {};
        Atom_Block *current_block = nullptr;
    };


    ZAPI void atom_table_init(Allocator *allocator, Atom_Table *at, i64 initial_capacity = ATOM_TABLE_INITIAL_CAPACITY);

    ZAPI Atom atom_get(Atom_Table *at, const char *cstr, i64 length);
    ZAPI Atom atom_get(Atom_Table *at, const char *cstr);
    ZAPI Atom atom_get(Atom_Table *at, const String_Ref &string_ref);

}