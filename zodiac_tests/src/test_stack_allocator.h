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

START_TESTS(stack_allocator_tests)
    DEFINE_TEST(Create_Free),
END_TESTS();

} }
