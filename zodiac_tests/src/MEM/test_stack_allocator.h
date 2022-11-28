#pragma once

#include <logger.h>
#include <memory/stack_allocator.h>
#include <test_common.h>

namespace Zodiac { namespace Stack_Allocator_Tests  {

static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_cap = 4;
    u64 alloc_size = sizeof(u64);
    u64 block_size = alloc_size * block_cap;

    Stack_Allocator state;
    stack_allocator_create(block_size, &state);

    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);
    munit_assert_ptr_not_null(state.head->memory);
    munit_assert_uint32(state.head->offset, ==, 0);
    munit_assert_uint32(state.head->size, ==, block_size);
    munit_assert_ptr_null(state.head->next);

    stack_allocator_destroy(&state);

    munit_assert_ptr_null(state.head);
    munit_assert_ptr_null(state.freelist);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_One(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_cap = 4;
    u64 alloc_size = sizeof(u64);
    u64 header_size = stack_allocator__header_size();
    u64 block_size = (alloc_size + header_size) * block_cap;

    Stack_Allocator state;
    stack_allocator_create(block_size, &state);

    void *memory = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);

    munit_assert_ptr(state.head->memory, <=, memory);
    munit_assert_ptr((u8 *)state.head->memory + state.head->size, >, memory);
    munit_assert_uint32(state.head->offset, ==, alloc_size + header_size);
    munit_assert_uint32(state.head->size, ==, block_size);

    munit_assert(stack_allocator_free(&state, memory));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size);
    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);
    munit_assert_uint32(state.head->offset, ==, 0);

    stack_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Alloc_Free_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_cap = 4;
    u64 alloc_size = sizeof(u64);
    u64 header_size = stack_allocator__header_size();
    u64 block_size = (alloc_size + header_size) * block_cap;

    Stack_Allocator state;
    stack_allocator_create(block_size, &state);

    auto first_block = state.head;

    void *memory = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);

    munit_assert_ptr(state.head->memory, <=, memory);
    munit_assert_ptr((u8 *)state.head->memory + state.head->size, >, memory);
    munit_assert_uint32(state.head->offset, ==, alloc_size + header_size);
    munit_assert_uint32(state.head->size, ==, block_size);

    void *memory2 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory2);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 2);
    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);

    munit_assert_ptr(state.head->memory, <=, memory2);
    munit_assert_ptr((u8 *)state.head->memory + state.head->size, >, memory2);
    munit_assert_uint32(state.head->offset, ==, (alloc_size + header_size) * 2);
    munit_assert_uint32(state.head->size, ==, block_size);

    void *memory3 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 3);
    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);

    munit_assert_ptr(state.head->memory, <=, memory3);
    munit_assert_ptr((u8 *)state.head->memory + state.head->size, >, memory3);
    munit_assert_uint32(state.head->offset, ==, (alloc_size + header_size) * 3);
    munit_assert_uint32(state.head->size, ==, block_size);

    void *memory4 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory4);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 4);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, 0);
    munit_assert_ptr_not_null(state.head);
    munit_assert_ptr_null(state.freelist);

    munit_assert_ptr(state.head->memory, <=, memory4);
    munit_assert_ptr((u8 *)state.head->memory + state.head->size, >, memory4);
    munit_assert_uint32(state.head->offset, ==, (alloc_size + header_size) * 4);
    munit_assert_uint32(state.head->size, ==, block_size);

    munit_assert(stack_allocator_free(&state, memory4));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 3);
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_null(state.freelist);

    munit_assert(stack_allocator_free(&state, memory3));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 2);
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_null(state.freelist);

    // Twice as large as the other allocations, so we are left over with the since of one header
    void *memory5 = stack_allocator_allocate(&state, alloc_size * 2);
    munit_assert_ptr_not_null(memory5);
    munit_assert_ptr_equal(memory5, memory3);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, header_size);

    munit_assert_ptr(state.head->memory, <=, memory5);
    munit_assert_ptr((u8 *)state.head->memory + state.head->size, >, memory5);
    munit_assert_uint32(state.head->offset, ==, alloc_size * 4 + header_size * 3);
    munit_assert_uint32(state.head->size, ==, block_size);

    munit_assert(stack_allocator_free(&state, memory5));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 2);
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_null(state.freelist);

    munit_assert(stack_allocator_free(&state, memory2));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 1);
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_null(state.freelist);

    munit_assert(stack_allocator_free(&state, memory));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size);
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_null(state.freelist);

    stack_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Invalid_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_cap = 4;
    u64 alloc_size = sizeof(u64);
    u64 header_size = stack_allocator__header_size();
    u64 block_size = (alloc_size + header_size) * block_cap;

    Stack_Allocator state;
    stack_allocator_create(block_size, &state);

    auto memory = stack_allocator_allocate(&state, alloc_size);
    auto memory2 = stack_allocator_allocate(&state, alloc_size);

    munit_assert_ptr_not_null(memory);
    munit_assert_ptr_not_null(memory2);
    munit_assert_ptr_not_equal(memory, memory2);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 2);

    ZINFO("The following error is intentionally caused by this test.");
    munit_assert_false(stack_allocator_free(&state, memory));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size) * 2);

    stack_allocator_destroy(&state);

    return MUNIT_OK;
}

static MunitResult Grow(const MunitParameter params[], void *user_data_or_fixture)
{
    u64 block_cap = 2;
    u64 alloc_size = sizeof(u64);
    u64 header_size = stack_allocator__header_size();
    u64 block_size = (alloc_size + header_size) * block_cap;

    Stack_Allocator state;
    stack_allocator_create(block_size, &state);

    auto first_block = state.head;

    auto memory = stack_allocator_allocate(&state, alloc_size);
    auto memory2 = stack_allocator_allocate(&state, alloc_size);

    munit_assert_ptr_not_null(memory);
    munit_assert_ptr_not_null(memory2);
    munit_assert_ptr_equal(state.head, first_block);

    munit_assert_uint64(stack_allocator_free_space(&state), ==, 0);

    auto memory3 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory3);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_not_equal(state.head, first_block);

    auto second_block = state.head;

    auto memory4 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory4);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, 0);
    munit_assert_ptr_not_equal(state.head, first_block);
    munit_assert_ptr_equal(state.head, second_block);

    auto memory5 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory5);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_not_equal(state.head, first_block);
    munit_assert_ptr_not_equal(state.head, second_block);

    auto third_block = state.head;

    ZINFO("The following error is intentionally caused by this test.");
    munit_assert_false(stack_allocator_free(&state, memory4));

    ZINFO("The following error is intentionally caused by this test.");
    munit_assert_false(stack_allocator_free(&state, memory2));

    munit_assert(stack_allocator_free(&state, memory5));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size);
    munit_assert_ptr_null(state.freelist);

    auto memory6 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_ptr_not_null(memory6);
    munit_assert_ptr_equal(memory5, memory6);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));

    munit_assert(stack_allocator_free(&state, memory6));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size);
    munit_assert_ptr_null(state.freelist);

    munit_assert(stack_allocator_free(&state, memory4));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size * 2 - (alloc_size + header_size));
    munit_assert_ptr_equal(state.head, second_block);
    munit_assert_ptr_equal(state.head->next, first_block);
    munit_assert_ptr_equal(state.freelist, third_block);

    auto memory7 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size);
    munit_assert_ptr_equal(state.head, second_block);
    munit_assert_ptr_equal(state.head->next, first_block);
    munit_assert_ptr_equal(state.freelist, third_block);

    auto memory8 = stack_allocator_allocate(&state, alloc_size);
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size - (alloc_size + header_size));
    munit_assert_ptr_equal(state.head, third_block);
    munit_assert_ptr_equal(state.head->next, second_block);
    munit_assert_ptr_equal(state.head->next->next, first_block);
    munit_assert_ptr_null(state.freelist);

    munit_assert(stack_allocator_free(&state, memory8));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size);
    munit_assert_ptr_equal(state.head, third_block);
    munit_assert_ptr_equal(state.head->next, second_block);
    munit_assert_ptr_equal(state.head->next->next, first_block);
    munit_assert_ptr_null(state.freelist);

    munit_assert(stack_allocator_free(&state, memory7));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size * 2 - (alloc_size + header_size));
    munit_assert_ptr_equal(state.head, second_block);
    munit_assert_ptr_equal(state.head->next, first_block);
    munit_assert_ptr_equal(state.freelist, third_block);

    munit_assert(stack_allocator_free(&state, memory3));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size * 2);
    munit_assert_ptr_equal(state.head, second_block);
    munit_assert_ptr_equal(state.head->next, first_block);
    munit_assert_ptr_equal(state.freelist, third_block);

    munit_assert(stack_allocator_free(&state, memory2));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size * 3 - (alloc_size + header_size));
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_equal(state.freelist, second_block);
    munit_assert_ptr_equal(state.freelist->next, third_block);

    munit_assert(stack_allocator_free(&state, memory));
    munit_assert_uint64(stack_allocator_free_space(&state), ==, block_size * 3);
    munit_assert_ptr_equal(state.head, first_block);
    munit_assert_ptr_equal(state.freelist, second_block);
    munit_assert_ptr_equal(state.freelist->next, third_block);

    stack_allocator_destroy(&state);

    return MUNIT_OK;
}

START_TESTS(stack_allocator_tests)
    DEFINE_TEST(Create_Free),
    DEFINE_TEST(Alloc_Free_One),
    DEFINE_TEST(Alloc_Free_Multi),
    DEFINE_TEST(Invalid_Free),
    DEFINE_TEST(Grow),
END_TESTS();

} }
