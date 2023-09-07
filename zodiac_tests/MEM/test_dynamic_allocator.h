#pragma once

#include "memory/dynamic_allocator.h"
#include "test_common.h"
#include "util/logger.h"

#include <munit/munit.h>

namespace Zodiac { namespace Dynamic_Allocator_Tests  {

static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_size = 512;

    Dynamic_Allocator state;
    ZINFO("The following warning is intentionally caused by this test");
    bool result = dynamic_allocator_create(block_size, &state);

    munit_assert(result);
    munit_assert_ptr_not_null(state.current_block);
    munit_assert_ptr_not_null(state.first_block);
    munit_assert_ptr_equal(state.current_block, state.first_block);
    munit_assert_uint64(state.initial_block_size, ==, block_size);

    munit_assert_uint64(freelist_free_space(&state.current_block->freelist), ==, block_size);
    munit_assert_ptr_not_null(state.current_block->memory);
    munit_assert_ptr_null(state.current_block->next);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size);

    dynamic_allocator_destroy(&state);

    munit_assert_ptr_null(state.current_block);
    munit_assert_ptr_null(state.first_block);
    munit_assert_uint64(state.initial_block_size, ==, 0);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_One(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_size = 512;
    u64 alloc_size = 64;
    auto header_size = dynamic_allocator__header_size();

    Dynamic_Allocator state;
    ZINFO("The following warning is intentionally caused by this test");
    bool result = dynamic_allocator_create(block_size, &state);
    munit_assert(result);

    auto memory = dynamic_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory, <, ((u64)state.first_block->memory) + block_size);

    dynamic_allocator_free(&state, memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size);

    dynamic_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_size = 512;
    u64 alloc_size = 64;
    auto header_size = dynamic_allocator__header_size();

    Dynamic_Allocator state;
    ZINFO("The following warning is intentionally caused by this test");
    bool result = dynamic_allocator_create(block_size, &state);
    munit_assert(result);

    auto memory = dynamic_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory, <, ((u64)state.first_block->memory) + block_size);


    auto memory2 = dynamic_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - ((alloc_size + header_size) * 2));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory2, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory2, <, ((u64)state.first_block->memory) + block_size);


    auto memory3 = dynamic_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - ((alloc_size + header_size) * 3));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory3, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory3, <, ((u64)state.first_block->memory) + block_size);


    dynamic_allocator_free(&state, memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - ((alloc_size + header_size) * 2));


    auto memory4 = dynamic_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory4);
    munit_assert_ptr_equal(memory2, memory4);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - ((alloc_size + header_size) * 3));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory4, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory4, <, ((u64)state.first_block->memory) + block_size);


    dynamic_allocator_free(&state, memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - ((alloc_size + header_size) * 2));


    dynamic_allocator_free(&state, memory4);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));


    dynamic_allocator_free(&state, memory4);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size);


    dynamic_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_Multi_Size(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_size = 512;
    auto header_size = dynamic_allocator__header_size();

    Dynamic_Allocator state;
    ZINFO("The following warning is intentionally caused by this test");
    bool result = dynamic_allocator_create(block_size, &state);
    munit_assert(result);

    u64 alloc_size = 64;
    auto memory = dynamic_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory, <, ((u64)state.first_block->memory) + block_size);


    u64 alloc_size2 = 32;
    auto memory2 = dynamic_allocator_allocate(&state, alloc_size2);
    munit_assert_ptr_not_null(memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + alloc_size2 + (header_size * 2)));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory2, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory2, <, ((u64)state.first_block->memory) + block_size);


    u64 alloc_size3 = 64;
    auto memory3 = dynamic_allocator_allocate(&state, alloc_size3);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + alloc_size2 + alloc_size3 + (header_size * 3)));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory3, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory3, <, ((u64)state.first_block->memory) + block_size);


    dynamic_allocator_free(&state, memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + alloc_size3 + (header_size * 2)));


    u64 alloc_size4 = 64;
    auto memory4 = dynamic_allocator_allocate(&state, alloc_size4);
    munit_assert_ptr_not_null(memory4);
    munit_assert_ptr_not_equal(memory2, memory4);
    munit_assert_uint64((u64)memory4, >, (u64)memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + alloc_size3 + alloc_size4 + (header_size * 3)));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory4, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory4, <, ((u64)state.first_block->memory) + block_size);


    u64 alloc_size5 = 32;
    auto memory5 = dynamic_allocator_allocate(&state, alloc_size5);
    munit_assert_ptr_not_null(memory5);
    munit_assert_ptr_equal(memory2, memory5);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size + alloc_size3 + alloc_size4 + alloc_size5 + (header_size * 4)));
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory5, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory5, <, ((u64)state.first_block->memory) + block_size);


    dynamic_allocator_free(&state, memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size3 + alloc_size4 + alloc_size5 + (header_size * 3)));


    dynamic_allocator_free(&state, memory4);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size3 + alloc_size5 + (header_size * 2)));


    dynamic_allocator_free(&state, memory3);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size - (alloc_size5 + header_size));

    dynamic_allocator_free(&state, memory5);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, block_size);


    dynamic_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_One_Aligned(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_size = 512;
    u64 alignment = 16;
    u64 header_size = dynamic_allocator__header_size();

    Dynamic_Allocator state;
    u64 total_size = block_size + (alignment - 1) + header_size;
    ZINFO("The following warning is intentionally caused by this test");
    bool result = dynamic_allocator_create(total_size, &state);
    munit_assert(result);

    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, total_size);


    void *memory = dynamic_allocator_allocate_aligned(&state, block_size, alignment);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(((u64)memory) % alignment, ==, 0);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, 0);
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory, <, ((u64)state.first_block->memory) + block_size);

    dynamic_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Grow(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_size = 512;
    u64 header_size = dynamic_allocator__header_size();

    Dynamic_Allocator state;
    u64 total_size = block_size + header_size;
    ZINFO("The following warning is intentionally caused by this test");
    bool result = dynamic_allocator_create(total_size, &state);
    munit_assert(result);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, total_size);

    // Fill up entire first block
    void *memory = dynamic_allocator_allocate(&state, block_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, 0);
    munit_assert_ptr_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory, >=, (u64)state.first_block->memory);
    munit_assert_uint64((u64)memory, <, ((u64)state.first_block->memory) + block_size);
    auto first_block = state.first_block;

    // This should allocate a second block and fill it up intirely
    void *memory2 = dynamic_allocator_allocate(&state, block_size);
    munit_assert_ptr_not_null(memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, 0);
    munit_assert_ptr_not_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory2, >=, (u64)state.current_block->memory);
    munit_assert_uint64((u64)memory2, <, ((u64)state.current_block->memory) + block_size);
    auto second_block = state.current_block;

    // This should allocate a third block and fill it half way + header_size
    u64 alloc_size3 = 256;
    void *memory3 = dynamic_allocator_allocate(&state, alloc_size3);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, total_size - alloc_size3 - header_size);
    munit_assert_ptr_not_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory3, >=, (u64)state.current_block->memory);
    munit_assert_uint64((u64)memory3, <, ((u64)state.current_block->memory) + block_size);
    auto third_block = state.current_block;


    // Allocate some more from the third block
    u64 alloc_size4 = 128;
    void *memory4 = dynamic_allocator_allocate(&state, alloc_size4);
    munit_assert_ptr_not_null(memory4);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, total_size - (alloc_size3 + alloc_size4) - (header_size * 2));
    munit_assert_ptr_not_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory4, >=, (u64)state.current_block->memory);
    munit_assert_uint64((u64)memory4, <, ((u64)state.current_block->memory) + block_size);
    munit_assert_ptr_equal(state.current_block, third_block);


    // Free the second block
    dynamic_allocator_free(&state, memory2);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 2) - (alloc_size3 + alloc_size4) - (header_size * 2));


    // Allocate some more, this should not fit in the current (third) block, so it should find the second block we freed before
    u64 alloc_size5 = 128;
    void *memory5 = dynamic_allocator_allocate(&state, alloc_size5);
    munit_assert_ptr_not_null(memory5);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 2) - (alloc_size3 + alloc_size4 + alloc_size5) - (header_size * 3));
    munit_assert_ptr_not_equal(state.first_block, state.current_block);
    munit_assert_uint64((u64)memory5, >=, (u64)second_block->memory);
    munit_assert_uint64((u64)memory5, <, ((u64)second_block->memory) + block_size);
    munit_assert_ptr_equal(state.current_block, third_block);


    // Free the first block
    dynamic_allocator_free(&state, memory);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 3) - (alloc_size3 + alloc_size4 + alloc_size5) - (header_size * 3));


    // Allocate an entire block again, reusing the first block
    u64 alloc_size6 = block_size;
    void *memory6 = dynamic_allocator_allocate(&state, alloc_size6);
    munit_assert_ptr_not_null(memory6);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 2) - (alloc_size3 + alloc_size4 + alloc_size5) - (header_size * 3));
    munit_assert_uint64((u64)memory6, >=, (u64)first_block->memory);
    munit_assert_uint64((u64)memory6, <, ((u64)first_block->memory) + block_size);
    munit_assert_ptr_equal(state.current_block, third_block);
    munit_assert_ptr_equal(memory, memory6);


    // Free the first/sixth block
    dynamic_allocator_free(&state, memory6);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 3) - (alloc_size3 + alloc_size4 + alloc_size5) - (header_size * 3));


    // Free the third allocation
    dynamic_allocator_free(&state, memory3);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 3) - (alloc_size4 + alloc_size5) - (header_size * 2));


    // Free the fourth allocation, this should free up the third block entirely
    dynamic_allocator_free(&state, memory4);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, (total_size * 3) - alloc_size5 - header_size);


    // Free the fifth allocation, this should free up the second block entirely
    dynamic_allocator_free(&state, memory5);
    munit_assert_uint64(dynamic_allocator_free_space(&state), ==, total_size * 3);


    dynamic_allocator_destroy(&state);

    return MUNIT_OK;
}

START_TESTS(dynamic_allocator_tests)
    DEFINE_TEST(Create_Free),
    DEFINE_TEST(Alloc_Free_One),
    DEFINE_TEST(Alloc_Free_Multi),
    DEFINE_TEST(Alloc_Free_Multi_Size),
    DEFINE_TEST(Alloc_Free_One_Aligned),
    DEFINE_TEST(Grow),
END_TESTS()

}}
