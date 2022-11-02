
#pragma once

#include <test_common.h>

#include <containers/dynamic_array.h>


namespace Zodiac { namespace Dynamic_Array_Tests {

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    // Default capacity
    {
        Dynamic_Array<int> array;
        dynamic_array_create(c_allocator(), &array);

        munit_assert_ptr_not_null(array.data);
        munit_assert_int(array.count, ==, 0);
        munit_assert_int(array.capacity, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY);
        munit_assert_ptr_equal(array.backing_allocator, c_allocator());

        dynamic_array_free(&array);
        munit_assert_ptr_null(array.data);
        munit_assert_int(array.count, ==, 0);
        munit_assert_int(array.capacity, ==, 0);
        munit_assert_ptr_null(array.backing_allocator);
    }

    // Custom capacity
    {
        const auto CAPACITY = 42;
        Dynamic_Array<int> array;
        dynamic_array_create(c_allocator(), &array, CAPACITY);

        munit_assert_ptr_not_null(array.data);
        munit_assert_int(array.count, ==, 0);
        munit_assert_int(array.capacity, ==, CAPACITY);
        munit_assert_ptr_equal(array.backing_allocator, c_allocator());

        dynamic_array_free(&array);
        munit_assert_ptr_null(array.data);
        munit_assert_int(array.count, ==, 0);
        munit_assert_int(array.capacity, ==, 0);
        munit_assert_ptr_null(array.backing_allocator);
    }

    // Zero capacity
    {
        const auto CAPACITY = 0;
        Dynamic_Array<int> array;
        dynamic_array_create(c_allocator(), &array, CAPACITY);

        munit_assert_ptr_null(array.data);
        munit_assert_int(array.count, ==, 0);
        munit_assert_int(array.capacity, ==, CAPACITY);
        munit_assert_ptr_equal(array.backing_allocator, c_allocator());

        dynamic_array_free(&array);
        munit_assert_ptr_null(array.data);
        munit_assert_int(array.count, ==, 0);
        munit_assert_int(array.capacity, ==, 0);
        munit_assert_ptr_null(array.backing_allocator);
    }

    return MUNIT_OK;
}

static MunitResult Indexing(const MunitParameter params[], void *user_data_or_fixture)
{

    Dynamic_Array<i64> array;
    dynamic_array_create(c_allocator(), &array);

    for (i64 i = 0; i < ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY; i++) {
        dynamic_array_append(&array, i);
    }


    for (i64 i = 0; i < ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY; i++) {
        munit_assert_int(array[i], ==, i);
    }

    for (i64 i = 0; i < ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY; i++) {
        array[i] = (i + 1) * 2;
    }

    for (i64 i = 0; i < ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY; i++) {
        munit_assert_int(array[i], ==, (i + 1) * 2);
    }

    dynamic_array_free(&array);

    return MUNIT_OK;
}

static MunitResult Growing(const MunitParameter params[], void *user_data_or_fixture)
{
    // Start with capacity = 0
    {
        Dynamic_Array<i64> array;
        dynamic_array_create(c_allocator(), &array, 0);

        munit_assert_int(array.capacity, ==, 0);
        munit_assert_int(array.count, ==, 0);

        dynamic_array_append(&array, (i64)42);

        munit_assert_int(array.capacity, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY);
        munit_assert_int(array.count, ==, 1);

        for (i64 i = 0; i < ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY - 1; i++) {
            dynamic_array_append(&array, i);
        }

        munit_assert_int(array.capacity, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY);
        munit_assert_int(array.count, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY);

        dynamic_array_append(&array, (i64)21);

        munit_assert_int(array.capacity, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY * 2);
        munit_assert_int(array.count, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY + 1);

        munit_assert_int(array[0], ==, 42);
        for (i64 i = 0; i < ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY - 1; i++) {
            munit_assert_int(array[i + 1], ==, i);
        }
        munit_assert_int(array[ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY], ==, 21);

        dynamic_array_free(&array);
    }

    // Start with capacity = 1
    {
        Dynamic_Array<i64> array;
        dynamic_array_create(c_allocator(), &array, 1);

        munit_assert_int(array.capacity, ==, 1);
        munit_assert_int(array.count, ==, 0);

        dynamic_array_append(&array, (i64)42);

        munit_assert_int(array.capacity, ==, 1);
        munit_assert_int(array.count, ==, 1);

        dynamic_array_append(&array, (i64)21);

        munit_assert_int(array.capacity, ==, 2);
        munit_assert_int(array.count, ==, 2);

        dynamic_array_append(&array, (i64)84);

        munit_assert_int(array.capacity, ==, 4);
        munit_assert_int(array.count, ==, 3);

        dynamic_array_free(&array);
    }

    // Default capacity
    {
        Dynamic_Array<i64> array;
        dynamic_array_create(c_allocator(), &array);

        munit_assert_int(array.capacity, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY);
        munit_assert_int(array.count, ==, 0);

        for (i64 i = 0; i < 12; i++) {
            dynamic_array_append(&array, i);
        }

        munit_assert_int(array.capacity, ==, ZODIAC_DYNAMIC_ARRAY_DEFAULT_CAPACITY * 2);
        munit_assert_int(array.count, ==, 12);

        for (i64 i = 0; i < 12; i++) {
            munit_assert_int(array[i], ==, i);
        }

        dynamic_array_free(&array);
    }

    return MUNIT_OK;
}

START_TESTS(dynamic_array_tests)
    DEFINE_TEST(Create_And_Free),
    DEFINE_TEST(Indexing),
    DEFINE_TEST(Growing),
END_TESTS()

}}
