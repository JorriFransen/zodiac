#include "ast.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve_error.h"
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

void flat_resolve_test(AST_File *file);

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;
    if (!Zodiac::type_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    // TODO: CLEANUP: Used in the resolver / resolve_error.h
    ctx = &c;

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

    flat_resolve_test(file);

    free(&dynamic_allocator, stream.data);

    return 0;
}

#define add_builtin_symbol(kind, atom) {                                                              \
    add_resolved_symbol(global_scope, (kind), (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), (atom), nullptr); \
}

void flat_resolve_test(AST_File *file)
{
    assert(file);

    Scope *global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);
    dynamic_array_create(&dynamic_allocator, &resolve_errors); // TODO: CLEANUP: Use the resolve_error_allocator for this?

    Resolver resolver;
    resolver_create(&resolver, ctx, global_scope);

    auto sym = add_resolved_symbol(global_scope, Symbol_Kind::TYPE, (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), atom_s64, nullptr);
    sym->builtin_type = &builtin_type_s64;

    // add_builtin_symbol(Symbol_Kind::TYPE, atom_s64);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_s8);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_u16);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_u32);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_r32);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_String);

    for (u64 i = 0; i < file->declarations.count; i++) {
        resolver_add_declaration(&resolver, file->declarations[i]);
    }

    resolve_names(&resolver);
    resolve_types(&resolver);

    for (u64 i = 0; i < resolve_errors.count; i++) {

        auto err = resolve_errors[i];

        if (!fatal_resolve_error) assert(!err.fatal);

        bool print = fatal_resolve_error == err.fatal;

        if (print) {
            printf("%s:%llu:%llu: error: %s\n", err.pos.name.data, err.pos.line, err.pos.index_in_line, err.message.data);
        }
    }
}


