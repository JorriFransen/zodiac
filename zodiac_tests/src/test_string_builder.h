#pragma once

#include "memory/zmemory.h"
#include "test_common.h"
#include "util/string_builder.h"

#include <munit/munit.h>

namespace Zodiac { namespace String_Builder_Tests {

static MunitResult Create_And_Destroy(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Builder sb;
    string_builder_create(&sb);

    munit_assert_ptr_equal(sb.allocator, &dynamic_allocator);
    munit_assert_uint64(sb.new_block_size, ==, ZSTRINGBUILDER_DEFAULT_BLOCK_SIZE);

    munit_assert_ptr_equal(sb.first_block, sb.current_block);
    munit_assert_ptr_not_null(sb.current_block->memory);
    munit_assert_uint64(sb.current_block->size, ==, ZSTRINGBUILDER_DEFAULT_BLOCK_SIZE);
    munit_assert_uint64(sb.current_block->used, ==, 0);
    munit_assert_ptr_null(sb.current_block->next_block);

    string_builder_destroy(&sb);
    munit_assert_ptr_null(sb.allocator);
    munit_assert_uint64(sb.new_block_size, ==, 0);
    munit_assert_ptr_null(sb.first_block);
    munit_assert_ptr_null(sb.current_block);

    return MUNIT_OK;
}

static MunitResult Single_Block(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Builder sb;
    string_builder_create(&sb);

    string_builder_append(&sb, "Hello, World!");

    String result = string_builder_to_string(&sb);
    munit_assert(string_equal(result, "Hello, World!"));
    free(sb.allocator, result.data);

    string_builder_append(&sb, " %i ", 42);

    result = string_builder_to_string(&sb);
    munit_assert(string_equal(result, "Hello, World! 42 "));
    free(sb.allocator, result.data);

    string_builder_destroy(&sb);

    return MUNIT_OK;
}

static MunitResult Grow(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Builder sb;
    string_builder_create(&sb, &dynamic_allocator, 4);

    string_builder_append(&sb, "ab");
    munit_assert_ptr_equal(sb.current_block, sb.first_block);

    string_builder_append(&sb, "cd");
    munit_assert_ptr_equal(sb.current_block, sb.first_block);

    string_builder_append(&sb, "ef");
    auto second_block = sb.current_block;
    munit_assert_ptr_not_equal(second_block, sb.first_block);

    string_builder_append(&sb, "gh");
    munit_assert_ptr_equal(sb.current_block, second_block);

    string_builder_append(&sb, "i");
    munit_assert_ptr_not_equal(sb.current_block, second_block);

    String result = string_builder_to_string(&sb);
    munit_assert(string_equal(result, "abcdefghi"));
    free(sb.allocator, result.data);

    string_builder_destroy(&sb);

    return MUNIT_OK;
}

START_TESTS(string_builder_tests)
    DEFINE_TEST(Create_And_Destroy),
    DEFINE_TEST(Single_Block),
    DEFINE_TEST(Grow),
END_TESTS()

}}
