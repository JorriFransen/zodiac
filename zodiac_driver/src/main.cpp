
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <parser.h>
#include <lexer.h>

#include <memory/allocator.h>
#include <memory/pool_allocator.h>

using namespace Zodiac;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Pool_Allocator expression_pool;
    pool_allocator_create(1024, sizeof(AST_Expression), &expression_pool);
    Allocator expr_allocator = pool_allocator_allocator(&expression_pool);

    Zodiac_Context c;
    zodiac_context_create(&expr_allocator, &c);

    Lexer lexer;
    lexer_create(&c, &lexer);
    // lexer_init_stream(&lexer, "1 + x * -3");
    // lexer_init_stream(&lexer, "abc[0].def()");
    lexer_init_stream(&lexer, "i = i + 1; some_func(a, b, c)");


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

