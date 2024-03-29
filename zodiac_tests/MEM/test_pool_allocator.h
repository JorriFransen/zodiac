#pragma once

#include "memory/pool_allocator.h"
#include "test_common.h"

#include <munit/munit.h>

namespace Zodiac { namespace Pool_Allocator_Tests {

static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;

    u64 capacity = 8;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size, &pool);

    munit_assert_uint64(pool.block_capacity, ==, capacity);
    munit_assert_uint64(pool.element_size, ==, element_size);
    munit_assert_ptr_not_null(pool.current_block->memory);
    munit_assert_ptr_not_null(pool.current_block->head);
    munit_assert_ptr_equal(pool.current_block->memory, pool.current_block->head);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity);

    pool_allocator_destroy(&pool);

    munit_assert_uint64(pool.block_capacity, ==, 0);
    munit_assert_uint64(pool.element_size, ==, 0);
    munit_assert_ptr_null(pool.current_block);
    munit_assert_ptr_null(pool.first_block);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_One(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;
    u64 capacity = 8;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size,  &pool);

    auto memory = (u64 *)pool_allocator_allocate(&pool);

    munit_assert_ptr_not_null(memory);
    munit_assert_uint64((u64)memory, >=, (u64)pool.first_block->memory);
    munit_assert_uint64((u64)memory, <, (u64)pool.first_block->memory + (capacity * element_size));
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 1);

    pool_allocator_destroy(&pool);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;
    u64 capacity = 8;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size, &pool);

    auto memory = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 1);

    auto memory2 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory2);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 2);

    auto memory3 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 3);

    auto memory4 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory4);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 4);

    pool_allocator_free(&pool, memory2);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 3);

    auto memory5 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory5);
    munit_assert_ptr_equal(memory2, memory5);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 4);

    auto memory6 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory6);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 5);

    auto memory7 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory7);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 6);

    auto memory8 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory8);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 7);

    auto memory9 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory9);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 8);


    pool_allocator_free(&pool, memory);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 7);

    auto memory10 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory10);
    munit_assert_ptr_equal(memory, memory10);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 8);

    pool_allocator_free(&pool, memory9);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 7);

    auto memory11 = pool_allocator_allocate(&pool);
    munit_assert_ptr_not_null(memory11);
    munit_assert_ptr_equal(memory9, memory11);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, capacity - 8);

    munit_assert_uint64((u64)memory, >=, (u64)pool.first_block->memory);
    munit_assert_uint64((u64)memory, <, (u64)pool.first_block->memory + (capacity * element_size));

    munit_assert_uint64((u64)memory9, >=, (u64)pool.first_block->memory);
    munit_assert_uint64((u64)memory9, <, (u64)pool.first_block->memory + (capacity * element_size));

    pool_allocator_destroy(&pool);

    return MUNIT_OK;
}

static MunitResult Grow(const MunitParameter params[], void *user_data_or_fixture)
{
    Pool_Allocator pool;
    u64 capacity = 4;
    u64 element_size = sizeof(u64);

    pool_allocator_create(capacity, element_size, &pool);

    auto first_block = pool.first_block;

    auto memory = pool_allocator_allocate(&pool);
    auto memory2 = pool_allocator_allocate(&pool);
    auto memory3 = pool_allocator_allocate(&pool);
    auto memory4 = pool_allocator_allocate(&pool);

    munit_assert_ptr_not_null(memory);
    munit_assert_ptr_not_null(memory2);
    munit_assert_ptr_not_null(memory3);
    munit_assert_ptr_not_null(memory4);

    munit_assert_ptr_equal(first_block, pool.current_block);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, 0);

    auto memory5 = pool_allocator_allocate(&pool);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, 3);
    auto second_block = pool.current_block;
    auto memory6 = pool_allocator_allocate(&pool);
    munit_assert_uint64(pool_allocator_free_capacity(&pool), ==, 2);

    munit_assert_ptr_not_null(memory5);
    munit_assert_ptr_not_null(memory6);

    munit_assert_ptr_not_equal(first_block, pool.current_block);
    munit_assert_ptr_equal(second_block, pool.current_block);

    return MUNIT_OK;
}

START_TESTS(pool_allocator_tests)
    DEFINE_TEST(Create_Free),
    DEFINE_TEST(Alloc_Free_One),
    DEFINE_TEST(Alloc_Free_Multi),
    DEFINE_TEST(Grow),
END_TESTS()

}}
