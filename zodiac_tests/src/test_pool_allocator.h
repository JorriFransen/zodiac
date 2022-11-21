#pragma once

#include <test_common.h>

#include <logger.h>
#include <memory/pool_allocator.h>
#include <memory/zmemory.h>

namespace Zodiac { namespace Pool_Allocator_Tests {

static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;

    u64 capacity = 8;
    u64 element_size = sizeof(u64);

    // Allocate memory ourselves
    {
        void *memory = zallocate(capacity * element_size);

        pool_allocator_create(capacity, element_size, memory, &pool);

        munit_assert_uint64(pool.capacity, ==, capacity);
        munit_assert_uint64(pool.element_size, ==, element_size);
        munit_assert_ptr_not_null(pool.memory);
        munit_assert_ptr_not_null(pool.head);
        munit_assert_ptr_equal(pool.memory, pool.head);
        munit_assert_false(pool.owns_memory);

        pool_allocator_destroy(&pool);

        munit_assert_uint64(pool.capacity, ==, 0);
        munit_assert_uint64(pool.element_size, ==, 0);
        munit_assert_ptr_null(pool.memory);
        munit_assert_ptr_null(pool.head);
        munit_assert_false(pool.owns_memory);

        zfree(memory);
    }

    // Let the pool allocate it's own memory
    {
        pool_allocator_create(capacity, element_size, nullptr, &pool);

        munit_assert_uint64(pool.capacity, ==, capacity);
        munit_assert_uint64(pool.element_size, ==, element_size);
        munit_assert_ptr_not_null(pool.memory);
        munit_assert_ptr_not_null(pool.head);
        munit_assert_ptr_equal(pool.memory, pool.head);
        munit_assert_true(pool.owns_memory);

        pool_allocator_destroy(&pool);

        munit_assert_uint64(pool.capacity, ==, 0);
        munit_assert_uint64(pool.element_size, ==, 0);
        munit_assert_ptr_null(pool.memory);
        munit_assert_ptr_null(pool.head);
        munit_assert_false(pool.owns_memory);
    }

    return MUNIT_OK;
}

static MunitResult Alloc_Free_One(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;
    u64 capacity = 8;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size, nullptr, &pool);

    auto memory = (u64 *)pool_allocator_allocate(&pool);

    munit_assert_ptr_not_null(memory);
    munit_assert_uint64((u64)memory, >=, (u64)pool.memory);
    munit_assert_uint64((u64)memory, <, (u64)pool.memory + (capacity * element_size));
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 1);

    pool_allocator_destroy(&pool);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;
    u64 capacity = 8;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size, nullptr, &pool);

    auto memory = pool_allocator_allocate(&pool);
    auto memory2 = pool_allocator_allocate(&pool);
    auto memory3 = pool_allocator_allocate(&pool);
    auto memory4 = pool_allocator_allocate(&pool);

    munit_assert_ptr_not_null(memory);
    munit_assert_ptr_not_null(memory2);
    munit_assert_ptr_not_null(memory3);
    munit_assert_ptr_not_null(memory4);

    pool_allocator_free(&pool, memory2);

    auto memory5 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory5);
    munit_assert_ptr_equal(memory2, memory5);

    auto memory6 = pool_allocator_allocate(&pool);
    auto memory7 = pool_allocator_allocate(&pool);
    auto memory8 = pool_allocator_allocate(&pool);
    auto memory9 = pool_allocator_allocate(&pool);

    munit_assert_ptr_not_null(memory6);
    munit_assert_ptr_not_null(memory7);
    munit_assert_ptr_not_null(memory8);
    munit_assert_ptr_not_null(memory9);

    pool_allocator_free(&pool, memory);

    auto memory10 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory10);
    munit_assert_ptr_equal(memory, memory10);

    pool_allocator_free(&pool, memory9);

    auto memory11 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory11);
    munit_assert_ptr_equal(memory9, memory11);

    munit_assert_uint64((u64)memory, >=, (u64)pool.memory);
    munit_assert_uint64((u64)memory, <, (u64)pool.memory + (capacity * element_size));

    munit_assert_uint64((u64)memory9, >=, (u64)pool.memory);
    munit_assert_uint64((u64)memory9, <, (u64)pool.memory + (capacity * element_size));

    pool_allocator_destroy(&pool);

    return MUNIT_OK;
}

static MunitResult Alloc_Over(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;
    u64 capacity = 2;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size, nullptr, &pool);

    auto memory = pool_allocator_allocate(&pool);
    auto memory2 = pool_allocator_allocate(&pool);

    munit_assert_ptr_not_null(memory);
    munit_assert_ptr_not_null(memory2);

    ZINFO("The following error is intentionally caused by this test");
    auto memory3 = pool_allocator_allocate(&pool);
    munit_assert_ptr_null(memory3);

    pool_allocator_free(&pool, memory);

    auto memory4 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory4);
    munit_assert_ptr(memory, ==, memory4);

    pool_allocator_destroy(&pool);

    return MUNIT_OK;
}

START_TESTS(pool_allocator_tests)
    DEFINE_TEST(Create_Free),
    DEFINE_TEST(Alloc_Free_One),
    DEFINE_TEST(Alloc_Free_Multi),
    DEFINE_TEST(Alloc_Over),
END_TESTS()

}}

