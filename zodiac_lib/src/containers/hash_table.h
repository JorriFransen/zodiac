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

ZAPI u64 hash_key(const char *str);
ZAPI u64 hash_key(s64 key);

template <typename T>
s64 hash_key(T *ptr) {
    return hash_s64((s64)ptr);
}

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

template <typename Key_Type, typename Value_Type>
void hash_table_add(Hash_Table<Key_Type, Value_Type> *ht, Key_Type key, Value_Type value)
{
    u64 hash = hash_key(key);
    assert(hash);
    u64 hash_index = hash % ht->capacity;
    s64 iteration_count = 0;

    while (iteration_count < ht->capacity)
    {
        if (ht->hashes[hash_index] == 0)
        {
            ht->hashes[hash_index] = hash;
            ht->keys[hash_index] = key;
            ht->values[hash_index] = value;
            return;
        }
        else
        {
            hash_index++;
            if (hash_index >= (u64)ht->capacity) hash_index = 0;
        }
        iteration_count++;
    }

    hash_table_grow(ht);
    hash_table_add(ht, key, value);
}

template <typename Key_Type, typename Value_Type>
void hash_table_grow(Hash_Table<Key_Type, Value_Type> *ht)
{
    auto new_cap = ht->capacity * 2;

    auto new_hashes_size = sizeof(u64) * new_cap;
    auto new_keys_size = sizeof(Key_Type) * new_cap;
    auto new_values_size = sizeof(Value_Type) * new_cap;

    auto new_total_size = new_hashes_size + new_keys_size + new_values_size;

    auto old_cap = ht->capacity;
    auto old_hashes = ht->hashes;
    auto old_keys = ht->keys;
    auto old_values = ht->values;

    auto new_mem = alloc_array<u8>(ht->allocator, new_total_size);

    ht->capacity = new_cap;
    ht->hashes = (u64*)(&new_mem[0]);
    ht->keys = (Key_Type*)(&new_mem[new_hashes_size]);
    ht->values = (Value_Type*)(&new_mem[new_hashes_size + new_keys_size]);

    memset(ht->hashes, 0, new_hashes_size);

    for (s64 i = 0; i < old_cap; i++) {
        if (old_hashes[i]) {
            hash_table_add(ht, old_keys[i], old_values[i]);
        }
    }

    free(ht->allocator, old_hashes);
}

template <typename Key_Type, typename Value_Type>
bool hash_table_find(Hash_Table<Key_Type, Value_Type> *ht, Key_Type key, Value_Type *vptr = nullptr)
{
    u64 hash = hash_key(key);
    u64 hash_index = hash % ht->capacity;
    s64 iteration_count = 0;

    while (iteration_count < ht->capacity)
    {
        if (ht->hashes[hash_index] == hash &&
            ht->keys_equal(ht->keys[hash_index], key))
        {
            if (vptr) *vptr = ht->values[hash_index];
            return true;
        }
        else if (ht->hashes[hash_index] == 0)
        {
            return false;
        }

        hash_index++;
        if (hash_index >= (u64)ht->capacity) hash_index = 0;
        iteration_count++;
    }

    return false;
}

template <typename Key_Type, typename Value_Type>
bool hash_table_find_key(Hash_Table<Key_Type, Value_Type> *ht, Value_Type value, Key_Type *kptr)
{
    for (s64 i = 0; i < ht->capacity; i++) {
        if (ht->hashes[i] != 0 &&
            ht->values[i] == value) {

            *kptr = ht->keys[i];
            return true;
        }
    }

    return false;
}

template <typename Key_Type, typename Value_Type>
void hash_table_reset(Hash_Table<Key_Type, Value_Type> *ht)
{
    memset(ht->hashes, 0, ht->capacity * sizeof(ht->hashes[0]));
}

template <typename Key_Type, typename Value_Type>
s64 hash_table_count(Hash_Table<Key_Type, Value_Type> *ht)
{
    s64 result = 0;

    for (u64 i = 0; i < ht->capacity; i++) {
        if (ht->hashes[i] != 0) result += 1;
    }

    return result;
}

}
