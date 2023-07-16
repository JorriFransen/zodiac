#pragma once

#include "lexer.h"
#include "memory/zmemory.h"
#include "test_common.h"
#include "util/logger.h"
#include "zodiac_context.h"

#include <munit/munit.h>

namespace Zodiac { namespace Lexer_Tests {

#define PRINT_TOK() {                                               \
    auto str_rep = tmp_token_str(lexer->token);                     \
    ZTRACE("Lexed token: %.*s", (int)str_rep.length, str_rep.data); \
}


#define ASSERT_TOK(knd)                             \
    munit_assert_int(lexer->token.kind, ==, (knd)); \
    PRINT_TOK();                                    \
    munit_assert(next_token(lexer));

#define ASSERT_TOK_NAME(str) {                             \
    munit_assert_int(lexer->token.kind, ==, TOK_NAME);     \
    munit_assert(string_equal(&lexer->token.atom, (str))); \
    PRINT_TOK();                                           \
    munit_assert(next_token(lexer));                       \
}

#define ASSERT_TOK_INT(num) {                             \
    munit_assert_int(lexer->token.kind, ==, TOK_INT);     \
    munit_assert_uint64(lexer->token.integer, ==, (num)); \
    PRINT_TOK();                                          \
    munit_assert(next_token(lexer));                      \
}

#define ASSERT_TOK_REAL(r) {                             \
    munit_assert_int(lexer->token.kind, ==, TOK_REAL);   \
    munit_assert_float(lexer->token.real.r32, ==, (r));  \
    munit_assert_double(lexer->token.real.r64, ==, (r)); \
    PRINT_TOK();                                         \
    munit_assert(next_token(lexer));                     \
}

#define ASSERT_TOK_KW(kw_atom) {                          \
    munit_assert_int(lexer->token.kind, ==, TOK_KEYWORD); \
    munit_assert(lexer->token.atom == (kw_atom));         \
    PRINT_TOK();                                          \
    munit_assert(next_token(lexer));                      \
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

    zfree(lexer);
    zfree(context);
}

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    return MUNIT_FAIL;
    // This test doesn't use the setup/teardown
    Zodiac_Context context;
    zodiac_context_create(&context);

    Lexer l;
    lexer_create(&context, &l);
    auto lexer = &l;

    const char *stream = "(";
    lexer_init_stream(lexer, stream, "test");

    munit_assert_ptr_equal(lexer->stream_start, stream);
    munit_assert_ptr_equal(lexer->stream, stream + 1);
    munit_assert_int(lexer->token.kind, ==, '(');
    munit_assert_uint64(lexer->token.atom.length, ==, 1);
    munit_assert(string_equal(&lexer->token.atom, stream));

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
    lexer_init_stream(lexer, stream, "test");

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
    lexer_init_stream(lexer, stream, "test");

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

    const char *stream = "0 10 42 18446744073709551615 2147483647 0x7FFFFFFF 0x7fffffff 0Xf 0b101 0B11110000";
    lexer_init_stream(lexer, stream, "test");

    ZTRACE("");
    ZTRACE("TEST: Lex_Int");
    ZTRACE("  stream: '%s'", stream);

    ASSERT_TOK_INT(0);
    ASSERT_TOK_INT(10);
    ASSERT_TOK_INT(42);
    ASSERT_TOK_INT(18446744073709551615ul);
    ASSERT_TOK_INT(2147483647);
    ASSERT_TOK_INT(2147483647);
    ASSERT_TOK_INT(2147483647);
    ASSERT_TOK_INT(15);
    ASSERT_TOK_INT(5);
    ASSERT_TOK_INT(240);

    ASSERT_TOK(TOK_EOF);

    ZTRACE("");

    return MUNIT_OK;
}

static MunitResult Lex_Float(const MunitParameter params[], void *user_data_or_fixture)
{
    auto lexer = (Lexer *)user_data_or_fixture;

    const char *stream = ".0 0. 0.0 4.2 .314 55. 0.54 4e3 4e+3 4e-3 4.2e4";
    lexer_init_stream(lexer, stream, "test");

    ZTRACE("");
    ZTRACE("TEST: Lex_Float");
    ZTRACE("  stream: '%s'", stream);

    ASSERT_TOK_REAL(0);
    ASSERT_TOK_REAL(0);
    ASSERT_TOK_REAL(0);
    ASSERT_TOK_REAL(4.2);
    ASSERT_TOK_REAL(0.314);
    ASSERT_TOK_REAL(55);
    ASSERT_TOK_REAL(0.54);
    ASSERT_TOK_REAL(4e3);
    ASSERT_TOK_REAL(4e+3);
    ASSERT_TOK_REAL(0.004);
    ASSERT_TOK_REAL(4.2e4);

    ASSERT_TOK(TOK_EOF);

    ZTRACE("");

    return MUNIT_OK;
}

static MunitResult Lex_Keyword(const MunitParameter params[], void *user_data_or_fixture)
{
    auto lexer = (Lexer *)user_data_or_fixture;

    const char *stream = "while sizeof struct sstruct or sizeofstruct";
    lexer_init_stream(lexer, stream, "test");

    ZTRACE("");
    ZTRACE("TEST: Lex_Keyword");
    ZTRACE("  stream: '%s'", stream);

    ASSERT_TOK_KW(keyword_while);
    ASSERT_TOK_KW(keyword_sizeof);
    ASSERT_TOK_KW(keyword_struct);
    ASSERT_TOK_NAME("sstruct");
    ASSERT_TOK_NAME("or");
    ASSERT_TOK_NAME("sizeofstruct");

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
   DEFINE_LEX_TEST(Lex_Float),
   DEFINE_LEX_TEST(Lex_Keyword),
END_TESTS()


#undef DEFINE_LEX_TEST

}}
