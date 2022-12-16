#pragma once

#include "zstring.h"

#include "test_common.h"

#include <munit/munit.h>

namespace Zodiac { namespace String_Tests {

static MunitResult String_Equal(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Ref a = "Hello, World!";
    String_Ref b = "Hello, World!";
    String_Ref c = "Hello, World";
    String_Ref d = "hello, World!";

    munit_assert_true(string_equal(a, b));
    munit_assert_false(string_equal(a, c));
    munit_assert_false(string_equal(a, d));

    munit_assert_false(string_equal(b, c));
    munit_assert_false(string_equal(b, d));

    munit_assert_false(string_equal(c, d));

    return MUNIT_OK;
}

static MunitResult String_Contains(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Ref a = "Hello, World!";
    String_Ref b = "Hello";
    String_Ref c = "World";
    String_Ref d = "hello";

    munit_assert_true(string_contains(a, b));
    munit_assert_true(string_contains(a, c));
    munit_assert_false(string_contains(a, d));

    munit_assert_false(string_contains(b, c));
    munit_assert_false(string_contains(b, d));

    munit_assert_false(string_contains(c, d));

    return MUNIT_OK;
}

static MunitResult String_Starts_With(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Ref a = "Hello, World!";
    String_Ref b = "Hello";
    String_Ref c = "World";
    String_Ref d = "hello";

    munit_assert_true(string_starts_with(a, b));
    munit_assert_false(string_starts_with(a, c));
    munit_assert_false(string_starts_with(a, d));

    munit_assert_false(string_starts_with(b, c));
    munit_assert_false(string_starts_with(b, d));

    munit_assert_false(string_starts_with(c, d));

    return MUNIT_OK;
}

static MunitResult String_Ends_With(const MunitParameter params[], void *user_data_or_fixture)
{
    String_Ref a = "Hello, World!";
    String_Ref b = " Hello";
    String_Ref c = "World!";
    String_Ref d = "World ";

    munit_assert_false(string_ends_with(a, b));
    munit_assert_true(string_ends_with(a, c));
    munit_assert_false(string_ends_with(a, d));

    munit_assert_false(string_ends_with(b, c));
    munit_assert_false(string_ends_with(b, d));

    munit_assert_false(string_ends_with(c, d));

    return MUNIT_OK;
}

START_TESTS(string_tests)
    DEFINE_TEST(String_Equal),
    DEFINE_TEST(String_Contains),
    DEFINE_TEST(String_Starts_With),
    DEFINE_TEST(String_Ends_With),
END_TESTS()

}}

