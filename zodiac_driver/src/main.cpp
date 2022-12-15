
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <lexer.h>
#include <parser.h>
#include <zodiac_context.h>

#include <memory/allocator.h>
#include <memory/linear_allocator.h>

#include "platform/filesystem.h"

#include <cstdarg>

using namespace Zodiac;

void resolve_test(Zodiac_Context *ctx, AST_File *file);

Zodiac_Context *ctx;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);
    ctx = &c;

    Lexer lexer;
    lexer_create(&c, &lexer);
    String stream = {};
    auto filename = "tests/test.zc";
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, filename, &stream);

    lexer_init_stream(&lexer, stream, filename);

    Parser parser;
    parser_create(&c, &lexer, &parser);

    AST_File *file = parse_file(&parser);
    if (parser.error) return 1;
    assert(file);

    // ast_print_file(file);

    resolve_test(&c, file);

    free(&dynamic_allocator, stream.data);

    return 0;
}

enum Symbol_Kind : u32
{
    SYM_INVALID,

    SYM_FUNC,

    SYM_VAR,
    SYM_PARAM,

    SYM_TYPE,
};

typedef u32 Symbol_Flags;

enum Symbol_Flag : Symbol_Flags 
{
    SYM_FLAG_NONE    = 0x00,
    SYM_FLAG_BUILTIN = 0x01,
    SYM_FLAG_GLOBAL  = 0x02,
};


struct Symbol
{
    Symbol_Kind kind;
    Symbol_Flags flags;

    Atom name;
    AST_Declaration *decl;    
};

struct Resolve_Error
{
    String message;
    Source_Pos pos;
    bool fatal;
};

Dynamic_Array<Resolve_Error> resolve_errors;
bool fatal_resolve_error = false;

void resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(ctx);

    auto at = &ctx->atoms;

    // dynamic_array_create(&dynamic_allocator, &name_resolved_symbols);
    dynamic_array_create(&dynamic_allocator, &resolve_errors);

    // add_symbol_(SYM_TYPE, SYM_FLAG_GLOBAL, atom_s64, nullptr);
    // add_symbol_(SYM_TYPE, SYM_FLAG_GLOBAL, atom_r32, nullptr);
    // add_symbol_(SYM_TYPE, SYM_FLAG_GLOBAL, atom_String, nullptr);
    // add_symbol_(SYM_TYPE, SYM_FLAG_GLOBAL, atom_get(at, "null"), nullptr);


    for (u64 i = 0; i < resolve_errors.count; i++) {
        auto err = resolve_errors[i];

        if (!fatal_resolve_error) assert(err.fatal == false);

        bool print = fatal_resolve_error == err.fatal;

        if (print) {
            printf("%s:%llu:%llu: error: %s\n", err.pos.name.data, err.pos.line, err.pos.index_in_line, err.message.data);
        }
    }

    // if (resolve_errors.count == 0) {
    //     for (u64 i = 0; i < name_resolved_symbols.count; i++) {
    //         // printf("%s\n", name_resolved_symbols[i].name.data);
    //         auto resolved_sym = name_resolved_symbols[i];

    //         if (resolved_sym.decl && (resolved_sym.flags & SYM_FLAG_GLOBAL)) {
    //             ast_print_declaration(resolved_sym.decl);
    //             printf("\n\n");
    //         }
    //     }
    // }
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args)
{
    Resolve_Error err;

    err.message = string_format(&ctx->resolve_error_allocator, fmt, args);
    err.pos = pos;
    err.fatal = fatal;

    dynamic_array_append(&resolve_errors, err);
}

void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(decl->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(stmt->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(expr->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(ts->pos, fatal, fmt, args);
    va_end(args);
}
