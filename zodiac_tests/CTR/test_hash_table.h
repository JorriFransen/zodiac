#pragma once

#include "containers/hash_table.h"
#include "test_common.h"

#include <munit/munit.h>

namespace Zodiac { namespace Hash_Table_Tests {

static bool int_eq_fn(int a, int b) {
    return a == b;
}

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    // Default key compare
    {
        Hash_Table<int, const char *> table;
        hash_table_create(c_allocator(), &table);

        munit_assert_ptr_not_null(table.hashes);
        munit_assert_ptr_not_null(table.keys);
        munit_assert_ptr_not_null(table.values);

        munit_assert_int64(table.capacity, ==, HASH_TABLE_INITIAL_CAPACITY);

        munit_assert_ptr((void *)table.keys_equal, ==, (void *)default_hash_table_equal<int>);
        munit_assert_ptr_equal(table.allocator, c_allocator());

        hash_table_free(&table);

        munit_assert_ptr_null(table.hashes);
        munit_assert_ptr_null(table.keys);
        munit_assert_ptr_null(table.values);

        munit_assert_int64(table.capacity, ==, 0);

        munit_assert_ptr_null((void *)table.keys_equal);
        munit_assert_ptr_null(table.allocator);
    }

    // Custom key compare
    {
        Hash_Table<int, const char *> table;
        hash_table_create(c_allocator(), &table, int_eq_fn);

        munit_assert_ptr_not_null(table.hashes);
        munit_assert_ptr_not_null(table.keys);
        munit_assert_ptr_not_null(table.values);

        munit_assert_int64(table.capacity, ==, HASH_TABLE_INITIAL_CAPACITY);

        munit_assert_ptr((void *)table.keys_equal, ==, (void *)int_eq_fn);
        munit_assert_ptr_equal(table.allocator, c_allocator());

        hash_table_free(&table);

        munit_assert_ptr_null(table.hashes);
        munit_assert_ptr_null(table.keys);
        munit_assert_ptr_null(table.values);

        munit_assert_int64(table.capacity, ==, 0);

        munit_assert_ptr_null((void *)table.keys_equal);
        munit_assert_ptr_null(table.allocator);
    }

    return MUNIT_OK;
}

static MunitResult Add_And_Find(const MunitParameter params[], void *user_data_or_fixture)
{
    Hash_Table<int, const char *> table;
    hash_table_create(c_allocator(), &table);

    auto s1 = "Really?";
    hash_table_add(&table, 42, s1);
    munit_assert_int64(hash_table_count(&table), ==, 1);

    auto s2 = "Yea!";
    hash_table_add(&table, 43, s2);
    munit_assert_int64(hash_table_count(&table), ==, 2);

    const char *val_1;
    bool find_res = hash_table_find(&table, 42, &val_1);
    munit_assert(find_res);
    munit_assert_ptr_equal(s1, val_1);
    munit_assert_string_equal(val_1, "Really?");

    const char *val_2;
    find_res = hash_table_find(&table, 43, &val_2);
    munit_assert(find_res);
    munit_assert_ptr_equal(s2, val_2);
    munit_assert_string_equal(val_2, "Yea!");

    hash_table_free(&table);

    return MUNIT_OK;
}

static MunitResult Grow(const MunitParameter params[], void *user_data_or_fixture)
{
    Hash_Table<int, int> table;
    hash_table_create(c_allocator(), &table);

    munit_assert_int64(table.capacity, ==, HASH_TABLE_INITIAL_CAPACITY);

    for (int i = 0; i < HASH_TABLE_INITIAL_CAPACITY; i++) {
        hash_table_add(&table, i, i);
    }

    munit_assert_int64(table.capacity, ==, HASH_TABLE_INITIAL_CAPACITY);
    munit_assert_int64(hash_table_count(&table), ==, HASH_TABLE_INITIAL_CAPACITY);

    for (int i = 0; i < HASH_TABLE_INITIAL_CAPACITY; i++) {
        hash_table_add(&table, i, i);
    }

    munit_assert_int64(table.capacity, ==, HASH_TABLE_INITIAL_CAPACITY * 2);
    munit_assert_int64(hash_table_count(&table), ==, HASH_TABLE_INITIAL_CAPACITY * 2);

    for (int i = 0; i < HASH_TABLE_INITIAL_CAPACITY; i++) {
        hash_table_add(&table, i * 3, i * 4);
    }

    munit_assert_int64(table.capacity, ==, HASH_TABLE_INITIAL_CAPACITY * 4);
    munit_assert_int64(hash_table_count(&table), ==, HASH_TABLE_INITIAL_CAPACITY * 3);

    hash_table_free(&table);

    return MUNIT_OK;
}

START_TESTS(hash_table_tests)
    DEFINE_TEST(Create_And_Free),
    DEFINE_TEST(Add_And_Find),
    DEFINE_TEST(Grow),
END_TESTS()

}}