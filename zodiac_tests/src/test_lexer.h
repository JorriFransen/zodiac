#pragma once

#include <test_common.h>

#include <lexer.h>

namespace Zodiac { namespace Lexer_Tests {

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    const char *stream = "(";
    Lexer lexer;
    lexer_create(stream, &lexer);

    munit_assert_ptr_equal(lexer.stream_start, stream);
    munit_assert_ptr_equal(lexer.stream, stream + 1);
    munit_assert_int(lexer.token.kind, ==, '(');
    munit_assert_ptr_equal(lexer.token.string.data, stream);
    munit_assert_uint64(lexer.token.string.length, ==, 1);

    lexer_destroy(&lexer);
    munit_assert_ptr_null(lexer.stream_start);
    munit_assert_ptr_null(lexer.stream);
    munit_assert_int(lexer.token.kind, ==, TOK_INVALID);

    return MUNIT_OK;
}

#define ASSERT_TOK(knd)                           \
    munit_assert_int(lexer.token.kind, ==, (knd))

#define ASSERT_TOK_NAME(str)                                 \
    munit_assert_int(lexer.token.kind, ==, TOK_NAME);        \
    munit_assert(string_equal(lexer.token.string, (str)));   \
    next_token(&lexer);

static MunitResult Lex_Name(const MunitParameter params[], void *user_data_or_fixture)
{
    const char *stream = "first SECOND Th3rd  _fourth \tfifth_56_sixth\n _Seventh_and_EIGHTH";
    Lexer lexer;
    lexer_create(stream, &lexer);

    ASSERT_TOK_NAME("first");
    ASSERT_TOK_NAME("SECOND");
    ASSERT_TOK_NAME("Th3rd");
    ASSERT_TOK_NAME("_fourth");
    ASSERT_TOK_NAME("fifth_56_sixth");
    ASSERT_TOK_NAME("_Seventh_and_EIGHTH");

    ASSERT_TOK(TOK_EOF);

    lexer_destroy(&lexer);

    return MUNIT_OK;
}

START_TESTS(lexer_tests)
   DEFINE_TEST(Create_And_Free),
   DEFINE_TEST(Lex_Name),
END_TESTS()

}}
