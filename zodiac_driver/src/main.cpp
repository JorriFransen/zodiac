
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <parser.h>
#include <lexer.h>

#include <memory/allocator.h>
#include <memory/linear_allocator.h>

using namespace Zodiac;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Linear_Allocator ast_linear_allocator;
    linear_allocator_create(MEBIBYTE(1), nullptr, &ast_linear_allocator);
    Allocator ast_allocator = linear_allocator_allocator(&ast_linear_allocator);

    Zodiac_Context c;
    zodiac_context_create(&ast_allocator, &c);

    Lexer lexer;
    lexer_create(&c, &lexer);
    const char *stream =
        "{"
          "i := 0;"
          "j : u32 = 5;"
          "k := u64(7);"
          "i = i * 5 + 1;"
          "y : s64;"
          "some_func(a, b, c);"
          "if i > 3 "
            "i_bigger_than_3();"
          " else if i > 2 "
            "i_bigger_than_2();"
          " else if i > 0 {"
            "i_bigger_than_0();"
          "} else {"
            "dunno();"
          "}"
          "return i;"
          "return;"
          "while i > 0 {"
            "i = i - 1;"
          "}"
          "while i > 0 i = i - 1;"
        "}";

    lexer_init_stream(&lexer, stream);


    Parser parser;
    parser_create(&c, &lexer, &parser);

    auto result = parse_statement(&parser);

    String_Builder sb;
    string_builder_create(&sb);

    ast_print_statement(&sb, result);
    string_builder_append(&sb, "\n");

    String ast_str = string_builder_to_string(&sb);
    printf("%.*s", (int)ast_str.length, ast_str.data);
    free(sb.allocator, ast_str.data);

    string_builder_destroy(&sb);

    return 0;
}

