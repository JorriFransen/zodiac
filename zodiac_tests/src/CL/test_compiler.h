#pragma once

#include <munit/munit.h>

#include "test_common.h"

#include "lexer.h"
#include "parser.h"
#include "resolve.h"
#include "scope.h"
#include "zodiac_context.h"

namespace Zodiac { namespace Compiler_Tests {

static MunitResult Return_0(const MunitParameter params[], void* user_data_or_fixture) {

    Zodiac_Context c;
    zodiac_context_create(&c);
    defer { zodiac_context_destroy(&c); };

    Lexer lexer;
    lexer_create(&c, &lexer);
    defer { lexer_destroy(&lexer); };

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 0;
        }
    )CODE_STR";

    lexer_init_stream(&lexer, code_string, "<test_code>");

    Parser parser;
    parser_create(&c, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);
    munit_assert_ptr_not_null(file);
    munit_assert_false(parser.error);

    Resolver resolver;
    resolver_create(&resolver, &c);
    defer { resolver_destroy(&resolver); };

    return MUNIT_OK;
}


START_TESTS(compiler_tests)
    DEFINE_TEST(Return_0),
END_TESTS()

}}
