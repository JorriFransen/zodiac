#pragma once

#include <test_common.h>

#include <lexer.h>
#include <logger.h>
#include <memory/zmemory.h>

namespace Zodiac { namespace Lexer_Tests {

#define PRINT_TOK() {                                               \
    auto str_rep = tmp_token_string(lexer->token);                  \
    ZTRACE("Lexed token: %.*s", (int)str_rep.length, str_rep.data); \
}


#define ASSERT_TOK(knd)                             \
    munit_assert_int(lexer->token.kind, ==, (knd)); \
    PRINT_TOK();                                    \
    next_token(lexer);

#define ASSERT_TOK_NAME(str) {                             \
    munit_assert_int(lexer->token.kind, ==, TOK_NAME);     \
    munit_assert(string_equal(lexer->token.atom, (str)));  \
    PRINT_TOK();                                           \
    next_token(lexer);                                    \
}

static void *lexer_test_setup(const MunitParameter params[], void *user_data)
{
    auto context = zallocate<Zodiac_Context>();
    zodiac_context_create(context);

    auto lexer = zallocate<Lexer>();
    lexer_create(context, lexer);

    return lexer;
}

static void lexer_test_tear_down(void *fixture)
{
    auto lexer = (Lexer *)fixture;
    auto context = lexer->context;

    lexer_destroy(lexer);
    zodiac_context_destroy(context);
}

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    // This test doesn't use the setup/teardown
    Zodiac_Context context;
    zodiac_context_create(&context);

    Lexer l;
    lexer_create(&context, &l);
    auto lexer = &l;

    const char *stream = "(";
    lexer_init_stream(lexer, stream);

    munit_assert_ptr_equal(lexer->stream_start, stream);
    munit_assert_ptr_equal(lexer->stream, stream + 1);
    munit_assert_int(lexer->token.kind, ==, '(');
    munit_assert_uint64(lexer->token.atom.length, ==, 1);
    munit_assert(string_equal(lexer->token.atom, stream));

    lexer_destroy(lexer);
    munit_assert_ptr_null(lexer->stream_start);
    munit_assert_ptr_null(lexer->stream);
    munit_assert_int(lexer->token.kind, ==, TOK_INVALID);

    zodiac_context_destroy(&context);

    return MUNIT_OK;
}

static MunitResult Lex_Name(const MunitParameter params[], void *user_data_or_fixture)
{
    auto lexer = (Lexer *)user_data_or_fixture;

    const char *stream = "first SECOND Th3rd  _fourth \tfifth_56_sixth\n _Seventh_and_EIGHTH";
    lexer_init_stream(lexer, stream);

    ZTRACE("");
    ZTRACE("TEST: Lex_Name");
    ZTRACE("  stream: '%s'", stream);

    ASSERT_TOK_NAME("first");
    ASSERT_TOK_NAME("SECOND");
    ASSERT_TOK_NAME("Th3rd");
    ASSERT_TOK_NAME("_fourth");
    ASSERT_TOK_NAME("fifth_56_sixth");
    ASSERT_TOK_NAME("_Seventh_and_EIGHTH");

    ASSERT_TOK(TOK_EOF);

    ZTRACE("");

    return MUNIT_OK;
}

static MunitResult Lex_Multi_Char(const MunitParameter params[], void *user_data_or_fixture)
{
    auto lexer = (Lexer *)user_data_or_fixture;

    const char *stream = "= = == ! = != < = <= > = >=";
    lexer_init_stream(lexer, stream);

    ZTRACE("");
    ZTRACE("TEST: Lex_Multi_Char");
    ZTRACE("  stream: '%s'", stream);

    ASSERT_TOK('=');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_EQ);
    ASSERT_TOK('!');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_NEQ);
    ASSERT_TOK('<');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_LTEQ);
    ASSERT_TOK('>');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_GTEQ);

    ASSERT_TOK(TOK_EOF);

    ZTRACE("");

    return MUNIT_OK;
}

static MunitResult Lex_Int(const MunitParameter params[], void *user_data_or_fixture)
{
    auto lexer = (Lexer *)user_data_or_fixture;

    const char *stream = "= = == ! = != < = <= > = >=";
    lexer_init_stream(lexer, stream);

    ZTRACE("");
    ZTRACE("TEST: Lex_Multi_Char");
    ZTRACE("  stream: '%s'", stream);

    ASSERT_TOK('=');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_EQ);
    ASSERT_TOK('!');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_NEQ);
    ASSERT_TOK('<');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_LTEQ);
    ASSERT_TOK('>');
    ASSERT_TOK('=');
    ASSERT_TOK(TOK_GTEQ);

    ASSERT_TOK(TOK_EOF);

    ZTRACE("");

    return MUNIT_OK;
}

#undef ASSERT_TOK
#undef ASSERT_TOK_NAME

#define DEFINE_LEX_TEST(fn) { \
    (char*)(#fn),             \
    (fn),                     \
    lexer_test_setup,         \
    lexer_test_tear_down,     \
    MUNIT_TEST_OPTION_NONE,   \
    nullptr,                  \
}

START_TESTS(lexer_tests)
   DEFINE_TEST(Create_And_Free),
   DEFINE_LEX_TEST(Lex_Name),
   DEFINE_LEX_TEST(Lex_Multi_Char),
   DEFINE_LEX_TEST(Lex_Int),
END_TESTS()


#undef DEFINE_LEX_TEST

}}
