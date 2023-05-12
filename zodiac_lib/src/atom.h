#pragma once

#include "defines.h"
#include "util/asserts.h"

namespace Zodiac
{

struct Allocator;

#define ATOM_TABLE_INITIAL_CAPACITY 64
#define ATOM_TABLE_INITIAL_BLOCK_SIZE 1024

struct String_Ref;

struct Atom
{
    const char *data;
    u64 length;
};

inline bool operator==(const Atom &lhs, const Atom &rhs)
{
    #if _DEBUG
        if (lhs.data == rhs.data) {
            assert(lhs.length == rhs.length);
            return true;
        } else {
            if (lhs.length != rhs.length) return false;

            for (u64 i = 0; i < lhs.length; i++) {
                if (lhs.data[i] != rhs.data[i]) return false;
            }

            assert(false && !"Returning false, but atoms seem to match...");
            return false;
        }
    #else
        return lhs.data == rhs.data;
    #endif
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

    u64 capacity = 0;

    Atom *atoms = nullptr;
    u64 *hashes = nullptr;

    Atom_Block first_block = {};
    Atom_Block *current_block = nullptr;
};


ZAPI void atom_table_init(Allocator *allocator, Atom_Table *at, u64 initial_capacity = ATOM_TABLE_INITIAL_CAPACITY);
ZAPI void atom_table_init(Atom_Table *at, u64 initial_capacity = ATOM_TABLE_INITIAL_CAPACITY);
ZAPI void atom_table_free(Atom_Table *at);

ZAPI Atom atom_get(Atom_Table *at, const char *cstr, u64 length);
ZAPI Atom atom_get(Atom_Table *at, const char *cstr);
ZAPI Atom atom_get(Atom_Table *at, const String_Ref &string_ref);

ZAPI u64 hash_key(const Atom &atom);

}
