#pragma once

#include "memory/linear_allocator.h"
#include "logger.h"
#include "test_common.h"

#include <munit/munit.h>

namespace Zodiac { namespace Linear_Allocator_Tests {


static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    Linear_Allocator allocator;

    {
        // Create with user supplied buffer
        u64 buf;
        linear_allocator_create(sizeof(u64), &buf, &allocator);

        munit_assert_uint64(allocator.size, ==, sizeof(u64));
        munit_assert_uint64(allocator.offset, ==, 0);
        munit_assert_ptr_equal(allocator.memory, &buf);
        munit_assert_false(allocator.owns_memory);
        munit_assert_uint64(linear_allocator_free_space(&allocator), ==, sizeof(u64));

        linear_allocator_destroy(&allocator);

        munit_assert_uint64(allocator.size, ==, 0);
        munit_assert_uint64(allocator.offset, ==, 0);
        munit_assert_ptr_null(allocator.memory);
    }

    {
        // Create without buffer, should create it's own
        linear_allocator_create(sizeof(u64), nullptr, &allocator);

        munit_assert_uint64(allocator.size, ==, sizeof(u64));
        munit_assert_uint64(allocator.offset, ==, 0);
        munit_assert_ptr_not_null(allocator.memory);
        munit_assert_true(allocator.owns_memory);
        munit_assert_uint64(linear_allocator_free_space(&allocator), ==, sizeof(u64));

        linear_allocator_destroy(&allocator);

        munit_assert_uint64(allocator.size, ==, 0);
        munit_assert_uint64(allocator.offset, ==, 0);
        munit_assert_ptr_null(allocator.memory);
    }

    return MUNIT_OK;
}

static MunitResult Alloc_All_Once(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 buf;
    u64 size = sizeof(buf);
    Linear_Allocator allocator;
    linear_allocator_create(size, &buf, &allocator);

    void *block = linear_allocator_allocate(&allocator, size);
    munit_assert_ptr_not_null(block);
    munit_assert_ptr_equal(block, &buf);
    munit_assert_uint64(allocator.offset, ==, size);
    munit_assert_uint64(allocator.offset, ==, allocator.size);
    munit_assert_uint64(linear_allocator_free_space(&allocator), ==, 0);

    linear_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Alloc_All_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 1024;
    u64 buffer_size = alloc_size * max_allocs;

    Linear_Allocator allocator;
    linear_allocator_create(buffer_size, nullptr, &allocator);
    munit_assert_uint64(linear_allocator_free_space(&allocator), ==, buffer_size);

    void *block;
    for (u64 i = 0; i < max_allocs; i++) {
        block = linear_allocator_allocate(&allocator, alloc_size);
        munit_assert_ptr_not_null(block);
        munit_assert_uint64(allocator.offset, ==, (i + 1) * alloc_size);
        munit_assert_uint64(linear_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * (i + 1)));
    }

    munit_assert_uint64(linear_allocator_free_space(&allocator), ==, 0);

    linear_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Alloc_All_Multi_Over(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 3;
    u64 buffer_size = alloc_size * max_allocs;

    Linear_Allocator allocator;
    linear_allocator_create(buffer_size, nullptr, &allocator);

    void *block;
    for (u64 i = 0; i < max_allocs; i++) {
        block = linear_allocator_allocate(&allocator, alloc_size);
        munit_assert_ptr_not_null(block);
        munit_assert_uint64(allocator.offset, ==, (i + 1) * alloc_size);
        munit_assert_uint64(linear_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * (i + 1)));
    }

    munit_assert_uint64(linear_allocator_free_space(&allocator), ==, 0);

    ZINFO("The following error is intentionally caused by this test");

    block = linear_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_null(block);
    munit_assert_int64(allocator.offset, ==, max_allocs * alloc_size);

    munit_assert_uint64(linear_allocator_free_space(&allocator), ==, 0);

    linear_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Alloc_All_Multi_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 1024;
    u64 buffer_size = alloc_size * max_allocs;

    Linear_Allocator allocator;
    linear_allocator_create(buffer_size, nullptr, &allocator);

    void *block;
    for (u64 i = 0; i < max_allocs; i++) {
        block = linear_allocator_allocate(&allocator, alloc_size);
        munit_assert_ptr_not_null(block);
        munit_assert_uint64(allocator.offset, ==, (i + 1) * alloc_size);
        munit_assert_uint64(linear_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * (i + 1)));
    }

    linear_allocator_free_all(&allocator);
    munit_assert_uint64(allocator.offset, ==, 0);
    munit_assert_uint64(linear_allocator_free_space(&allocator), ==, buffer_size);

    linear_allocator_destroy(&allocator);

    return MUNIT_OK;
}

START_TESTS(linear_allocator_tests)
    DEFINE_TEST(Create_Free),
    DEFINE_TEST(Alloc_All_Once),
    DEFINE_TEST(Alloc_All_Multi),
    DEFINE_TEST(Alloc_All_Multi_Over),
    DEFINE_TEST(Alloc_All_Multi_Free),
END_TESTS()

}}
