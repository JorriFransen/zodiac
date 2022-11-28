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

static MunitResult Lex_Name(const MunitParameter params[], void *user_data_or_fixture)
{
    const char *stream = "first SECOND Th3rd  _fourth \tfifth_56_sixth\n _Seventh_and_EIGHTH";
    Lexer lexer;
    lexer_create(stream, &lexer);

    munit_assert_int(lexer.token.kind, ==, TOK_NAME);
    munit_assert(string_equal(lexer.token.string, "first"));

    next_token(&lexer);

    munit_assert_int(lexer.token.kind, ==, TOK_NAME);
    munit_assert(string_equal(lexer.token.string, "SECOND"));

    next_token(&lexer);

    munit_assert_int(lexer.token.kind, ==, TOK_NAME);
    munit_assert(string_equal(lexer.token.string, "Th3rd"));

    next_token(&lexer);

    munit_assert_int(lexer.token.kind, ==, TOK_NAME);
    munit_assert(string_equal(lexer.token.string, "_fourth"));

    next_token(&lexer);

    munit_assert_int(lexer.token.kind, ==, TOK_NAME);
    munit_assert(string_equal(lexer.token.string, "fifth_56_sixth"));

    next_token(&lexer);

    munit_assert_int(lexer.token.kind, ==, TOK_NAME);
    munit_assert(string_equal(lexer.token.string, "_Seventh_and_EIGHTH"));

    next_token(&lexer);
    munit_assert_int(lexer.token.kind, ==, TOK_EOF);

    lexer_destroy(&lexer);

    return MUNIT_OK;
}

START_TESTS(lexer_tests)
   DEFINE_TEST(Create_And_Free),
   DEFINE_TEST(Lex_Name),
END_TESTS()

}}
