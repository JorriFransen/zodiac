
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <lexer.h>
#include <parser.h>
#include <zodiac_context.h>

#include <memory/allocator.h>
#include <memory/linear_allocator.h>

using namespace Zodiac;

int main() {

    // Comment...
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

        "Vec2 :: struct {"
        "  x: s32;"
        "  y: s32;"
        "}"

        "AABB :: struct {"
        "  top_left, dim: Vec2;"
        "}"

        "Value :: union {"
        "  int_value: s32;"
        "  float_value: r32;"
        "}"

        "var := 42;"
        "var2 : u16 = 8;"
        "var3 : s8;"
        "c_var :: 5;"
        "c_var2 : u32 : 2;"
        
        "add_fn :: (a: u64, b: u64) -> u64 {"
          "result := a + b;"
          "return result;"
        "}"

        "main :: () -> s64 {"
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

    Dynamic_Array<AST_Declaration *> global_decls;
    dynamic_array_create(c.ast_allocator, &global_decls);

    while (!is_token(&parser, TOK_EOF)) {
        auto decl = parse_declaration(&parser);
        assert(decl);
        dynamic_array_append(&global_decls, decl);
    }

    String_Builder sb;
    string_builder_create(&sb);

    for (u64 i = 0; i < global_decls.count; i++) {
        ast_print_declaration(&sb, global_decls[i]);
        string_builder_append(&sb, "\n\n");
    }

    String ast_str = string_builder_to_string(&sb);
    printf("%.*s", (int)ast_str.length, ast_str.data);
    free(sb.allocator, ast_str.data);

    string_builder_destroy(&sb);

    return 0;
}

