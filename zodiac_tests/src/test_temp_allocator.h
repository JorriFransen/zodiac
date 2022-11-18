
#pragma once

#include <test_common.h>

#include <memory/temporary_allocator.h>

namespace Zodiac { namespace Temp_Allocator_Tests {


static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    Temporary_Allocator allocator;

    u64 buf;

    // Create with user supplied buffer
    {
        temporary_allocator_create(sizeof(buf), &buf, &allocator);
        munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, sizeof(buf));
        munit_assert_ptr_not_null(allocator.linear_allocator.memory);
        munit_assert_ptr_equal(allocator.linear_allocator.memory, &buf);

        temporary_allocator_destroy(&allocator);

        munit_assert_ptr_null(allocator.linear_allocator.memory);
    }

    // Create without buffer, should creat it's own
    {
        temporary_allocator_create(sizeof(buf), nullptr, &allocator);

        munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, sizeof(buf));
        munit_assert_ptr_not_null(allocator.linear_allocator.memory);
        munit_assert_ptr_not_equal(allocator.linear_allocator.memory, &buf);

        temporary_allocator_destroy(&allocator);

        munit_assert_ptr_null(allocator.linear_allocator.memory);
    }

    return MUNIT_OK;
}

static MunitResult Alloc_All_Once(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 buf;
    u64 size = sizeof(buf);
    Temporary_Allocator allocator;
    temporary_allocator_create(size, &buf, &allocator);

    void *block = temporary_allocator_allocate(&allocator, size);
    munit_assert_ptr_not_null(block);
    munit_assert_ptr_equal(block, &buf);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, 0);

    temporary_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Alloc_All_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 1024;
    u64 buffer_size = alloc_size * max_allocs;

    Temporary_Allocator allocator;
    temporary_allocator_create(buffer_size, nullptr, &allocator);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size);

    void *block;
    for (u64 i = 0; i < max_allocs; i++) {
        block = temporary_allocator_allocate(&allocator, alloc_size);
        munit_assert_ptr_not_null(block);
        munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * (i + 1)));
    }

    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, 0);

    temporary_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Alloc_All_Multi_Over(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 3;
    u64 buffer_size = alloc_size * max_allocs;

    Temporary_Allocator allocator;
    temporary_allocator_create(buffer_size, nullptr, &allocator);

    void *block;
    for (u64 i = 0; i < max_allocs; i++) {
        block = temporary_allocator_allocate(&allocator, alloc_size);
        munit_assert_ptr_not_null(block);
        munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * (i + 1)));
    }

    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, 0);

    zodiac_info("The following error is intentionally caused by this test");
    block = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_null(block);

    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, 0);

    temporary_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Alloc_All_Multi_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 1024;
    u64 buffer_size = alloc_size * max_allocs;

    Temporary_Allocator allocator;
    temporary_allocator_create(buffer_size, nullptr, &allocator);

    void *block;
    for (u64 i = 0; i < max_allocs; i++) {
        block = temporary_allocator_allocate(&allocator, alloc_size);
        munit_assert_ptr_not_null(block);
        munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * (i + 1)));
    }

    temporary_allocator_reset(&allocator);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size);

    temporary_allocator_destroy(&allocator);

    return MUNIT_OK;
}

static MunitResult Marks(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 alloc_size = sizeof(u64);
    u64 max_allocs = 4;
    u64 buffer_size = alloc_size * max_allocs;

    Temporary_Allocator allocator;
    temporary_allocator_create(buffer_size, nullptr, &allocator);

    void *memory = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - alloc_size);

    Temporary_Allocator_Mark mark = temporary_allocator_get_mark(&allocator);
    munit_assert_uint64(mark.offset, ==, alloc_size);

    void *memory2 = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_not_null(memory2);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 2));
    munit_assert_ptr_not_equal(memory2, memory);

    Temporary_Allocator_Mark mark2 = temporary_allocator_get_mark(&allocator);
    munit_assert_uint64(mark2.offset, ==, alloc_size * 2);

    void *memory3 = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 3));
    munit_assert_ptr_not_equal(memory3, memory);
    munit_assert_ptr_not_equal(memory3, memory2);

    Temporary_Allocator_Mark mark3 = temporary_allocator_get_mark(&allocator);
    munit_assert_uint64(mark3.offset, ==, alloc_size * 3);

    void *memory4 = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_not_null(memory4);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 4));
    munit_assert_ptr_not_equal(memory4, memory);
    munit_assert_ptr_not_equal(memory4, memory2);
    munit_assert_ptr_not_equal(memory4, memory3);

    Temporary_Allocator_Mark mark4 = temporary_allocator_get_mark(&allocator);
    munit_assert_uint64(mark4.offset, ==, alloc_size * 4);

    temporary_allocator_reset(&allocator, mark3);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 3));

    void *memory5 = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_not_null(memory5);
    munit_assert_int64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 4));
    munit_assert_ptr_not_equal(memory5, memory);
    munit_assert_ptr_not_equal(memory5, memory2);
    munit_assert_ptr_not_equal(memory5, memory3);
    munit_assert_ptr_equal(memory5, memory4);

    temporary_allocator_reset(&allocator, mark2);
    munit_assert_uint64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 2));

    void *memory6 = temporary_allocator_allocate(&allocator, alloc_size);
    munit_assert_ptr_not_null(memory6);
    munit_assert_int64(temporary_allocator_free_space(&allocator), ==, buffer_size - (alloc_size * 3));
    munit_assert_ptr_not_equal(memory6, memory);
    munit_assert_ptr_not_equal(memory6, memory2);
    munit_assert_ptr_equal(memory6, memory3);
    munit_assert_ptr_not_equal(memory6, memory4);
    munit_assert_ptr_not_equal(memory6, memory5);

    return MUNIT_OK;
}

START_TESTS(temp_allocator_tests)
    DEFINE_TEST(Create_Free),
    DEFINE_TEST(Alloc_All_Once),
    DEFINE_TEST(Alloc_All_Multi),
    DEFINE_TEST(Alloc_All_Multi_Over),
    DEFINE_TEST(Alloc_All_Multi_Free),
    DEFINE_TEST(Marks),
END_TESTS()

}}
