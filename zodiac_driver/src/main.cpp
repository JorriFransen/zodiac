
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <lexer.h>
#include <parser.h>
#include <zodiac_context.h>

#include <memory/allocator.h>
#include <memory/linear_allocator.h>

#include "platform/filesystem.h"

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
    String stream = {};
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, "tests/test.zc", &stream);

    lexer_init_stream(&lexer, stream, "test");

    // while (!is_token(&lexer, TOK_EOF)) {
    //     print_token(lexer.token);
    //     next_token(&lexer);
    // }

    // return 0;

    Parser parser;
    parser_create(&c, &lexer, &parser);

    Dynamic_Array<AST_Declaration *> global_decls;
    dynamic_array_create(c.ast_allocator, &global_decls);

    while (!is_token(&parser, TOK_EOF)) {
        auto decl = parse_declaration(&parser);
        if (parser.error) break;

        dynamic_array_append(&global_decls, decl);
    }

    if (parser.error) return 1;

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

