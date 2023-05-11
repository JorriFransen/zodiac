#pragma once

#include "defines.h"
#include "memory/allocator.h"
#include "util/asserts.h"

namespace Zodiac {

#define HASH_TABLE_INITIAL_CAPACITY 16

template <typename Key_Type>
using Hash_Table_Keys_Equal_FN = bool (*)(Key_Type a, Key_Type b);

template <typename Key_Type>
bool default_hash_table_equal(Key_Type a, Key_Type b) {
    return a == b;
}

template <typename Key_Type, typename Value_Type>
struct Hash_Table
{
    u64 *hashes = nullptr;
    Key_Type *keys = nullptr;
    Value_Type *values = nullptr;

    s64 capacity = 0;

    Hash_Table_Keys_Equal_FN<Key_Type> keys_equal = nullptr;
    Allocator *allocator = {};
};

template <typename Key_Type, typename Value_Type>
void hash_table_create(Allocator *allocator, Hash_Table<Key_Type, Value_Type> *hash_table, Hash_Table_Keys_Equal_FN<Key_Type> keys_equal = default_hash_table_equal)
{
    assert(allocator);
    assert(hash_table);
    assert(keys_equal);

    auto hashes_size = sizeof(u64) * HASH_TABLE_INITIAL_CAPACITY;
    auto keys_size = sizeof(Key_Type) * HASH_TABLE_INITIAL_CAPACITY;
    auto values_size = sizeof(Value_Type) * HASH_TABLE_INITIAL_CAPACITY;

    auto total_size = hashes_size + keys_size + values_size;

    u8 *mem = alloc_array<u8>(allocator, total_size);
    assert(mem);

    hash_table->hashes = (u64 *)mem;
    hash_table->keys = (Key_Type*)(&mem[hashes_size]);
    hash_table->values = (Value_Type*)(&mem[hashes_size + keys_size]);

    memset(hash_table->hashes, 0, hashes_size);

    hash_table->capacity = HASH_TABLE_INITIAL_CAPACITY;
    hash_table->keys_equal = keys_equal;
    hash_table->allocator = allocator;
}

template <typename Key_Type, typename Value_Type>
void hash_table_free(Hash_Table<Key_Type, Value_Type> *hash_table)
{
    assert(hash_table);
    assert(hash_table->allocator);

    assert(hash_table->hashes);

    free(hash_table->allocator, hash_table->hashes);

    *hash_table = {};
}

}
