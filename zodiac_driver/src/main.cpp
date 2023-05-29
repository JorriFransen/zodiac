#include "ast.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

#include <stdio.h>

using namespace Zodiac;

void flat_resolve_test(Zodiac_Context *ctx, AST_File *file);

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;
    if (!Zodiac::type_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    Lexer lexer;
    lexer_create(&c, &lexer);
    String stream = {};

    auto filename = "tests/simple.zc";
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, filename, &stream);
    assert(read_result);
    if (!read_result) {
        return 1;
    }

    lexer_init_stream(&lexer, stream, filename);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    AST_File *file = parse_file(&parser);
    if (parser.error) return 1;;
    assert(file);

    flat_resolve_test(&c, file);

    free(&dynamic_allocator, stream.data);

    return 0;
}

#define add_builtin_type_symbol(type) { \
    auto sym = add_resolved_symbol(ctx, global_scope, Symbol_Kind::TYPE, (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), atom_##type, nullptr); \
    sym->builtin_type = &builtin_type_##type; \
}

void flat_resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(file);

    Scope *global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);

    Resolver resolver;
    resolver_create(&resolver, ctx, global_scope);

    add_builtin_type_symbol(u64);
    add_builtin_type_symbol(s64);
    add_builtin_type_symbol(u32);
    add_builtin_type_symbol(s32);
    add_builtin_type_symbol(u16);
    add_builtin_type_symbol(s16);
    add_builtin_type_symbol(u8);
    add_builtin_type_symbol(s8);

    add_builtin_type_symbol(r32);
    add_builtin_type_symbol(r64);

    add_builtin_type_symbol(String);

    for (u64 i = 0; i < file->declarations.count; i++) {
        resolver_add_declaration(ctx, &resolver, file->declarations[i]);
    }

    bool names_done = resolve_names(&resolver);

    if (names_done) resolve_types(&resolver);

    for (u64 i = 0; i < ctx->errors.count; i++) {

        auto err = ctx->errors[i];

        if (!ctx->fatal_resolve_error) assert(!err.fatal);

        bool print = ctx->fatal_resolve_error == err.fatal;

        if (print) {
            auto start = err.source_range.start;
            printf("%s:%llu:%llu: error: %s\n", start.name.data, start.line, start.index_in_line, err.message.data);
        }
    }
}


