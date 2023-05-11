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

START_TESTS(hash_table_tests)
    DEFINE_TEST(Create_And_Free),
END_TESTS()

}}