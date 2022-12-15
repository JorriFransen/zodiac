
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

enum class Symbol_Kind : u16
{
    INVALID,

    FUNC,

    VAR,
    PARAM,

    TYPE,
};

enum class Symbol_State : u16
{
    UNRESOLVED,
    RESOLVING,
    RESOLVED,
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
    Symbol_State state;
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

Dynamic_Array<Symbol> symbols;
u64 name_resolved_count = 0;
Dynamic_Array<Resolve_Error> resolve_errors;
bool fatal_resolve_error = false;

bool add_unresolved_symbol(Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);
bool add_resolved_symbol(Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);
bool add_unresolved_decl_symbol(AST_Declaration *decl, bool global);

Symbol *get_symbol(Atom name);

bool name_resolve_decl_(AST_Declaration *decl);
bool name_resolve_stmt_(AST_Statement *stmt);
bool name_resolve_expr_(AST_Expression *expr);
bool name_resolve_ts_(AST_Type_Spec *ts);

#define name_resolve_decl(decl) {    \
    if (!name_resolve_decl_(decl)) { \
        result = false;              \
        goto exit;                   \
    }                                \
}

#define name_resolve_stmt(stmt) {    \
    if (!name_resolve_stmt_(stmt)) { \
        result = false;              \
        goto exit;                   \
    }                                \
}

#define name_resolve_expr(expr) {    \
    if (!name_resolve_expr_(expr)) { \
        result = false;              \
        goto exit;                   \
    }                                \
}

#define name_resolve_ts(ts) {    \
    if (!name_resolve_ts_(ts)) { \
        result = false;          \
        goto exit;               \
    }                            \
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args);
void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...);

#define resolve_error(node, fmt, ...) resolve_error_((node), false, fmt, ##__VA_ARGS__);

#define fatal_resolve_error(node, fmt, ...) {         \
    fatal_resolve_error = true;                       \
    resolve_error_((node), true, fmt, ##__VA_ARGS__); \
}

void resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(ctx);

    auto at = &ctx->atoms;

    dynamic_array_create(&dynamic_allocator, &symbols);
    dynamic_array_create(&dynamic_allocator, &resolve_errors);

    Symbol_Flags builtin_global_flags = SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN;

    add_resolved_symbol(Symbol_Kind::TYPE, builtin_global_flags, atom_s64, nullptr);
    add_resolved_symbol(Symbol_Kind::TYPE, builtin_global_flags, atom_r32, nullptr);
    add_resolved_symbol(Symbol_Kind::TYPE, builtin_global_flags, atom_String, nullptr);
    add_resolved_symbol(Symbol_Kind::TYPE, builtin_global_flags, atom_get(at, "null"), nullptr);

    auto file_decls = dynamic_array_copy(&file->declarations, &dynamic_allocator);

    // Add global symbols
    for (u64 i = 0; i < file_decls.count; i++) {
        auto decl = file_decls[i];
        add_unresolved_decl_symbol(decl, true);
    }

    bool progress = true;
    bool done = false;

    while (progress && !done && !fatal_resolve_error) {

        auto last_name_resolved_count = name_resolved_count;

        done = true;
        for (u64 i = 0; i < file_decls.count; i++) {
            
            AST_Declaration *decl = file_decls[i];

            if (decl) {
                if (name_resolve_decl_(decl)) {
                    // Set the decl to null instead of removing it (don't want to do unordered removal)
                    file_decls[i] = nullptr;

                } else {
                    done = false;
                }
            }
        }

        progress = last_name_resolved_count < name_resolved_count;

        if (progress && !fatal_resolve_error) {
            resolve_errors.count = 0;
            temporary_allocator_reset(&ctx->resolve_error_allocator_state);
        }
    }

    dynamic_array_free(&file_decls);

    for (u64 i = 0; i < resolve_errors.count; i++) {
        auto err = resolve_errors[i];

        if (!fatal_resolve_error) assert(err.fatal == false);

        bool print = fatal_resolve_error == err.fatal;

        if (print) {
            printf("%s:%llu:%llu: error: %s\n", err.pos.name.data, err.pos.line, err.pos.index_in_line, err.message.data);
        }
    }

    if (resolve_errors.count == 0) {
        for (u64 i = 0; i < symbols.count; i++) {

            auto sym = symbols[i];
            if (sym.state == Symbol_State::RESOLVED && sym.decl && (sym.flags & SYM_FLAG_GLOBAL)) {
                ast_print_declaration(sym.decl);
                printf("\n\n");
            }
        }
    }
}

bool add_unresolved_symbol(Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(kind != Symbol_Kind::INVALID);
    assert(decl);

    if (get_symbol(name)) {
        fatal_resolve_error(decl, "Redeclaration of symbol: '%s'", name.data);
        return false;
    }

    Symbol sym = {
        kind,
        Symbol_State::UNRESOLVED,
        flags,
        name,
        decl,
    };

    dynamic_array_append(&symbols, sym);
    return true;
}

bool add_resolved_symbol(Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(kind != Symbol_Kind::INVALID);
    assert(decl || (flags & SYM_FLAG_BUILTIN));

    if (get_symbol(name)) {
        fatal_resolve_error(decl, "Redeclaration of symbol: '%s'", name.data);
        return false;
    }

    Symbol sym = {
        kind,
        Symbol_State::RESOLVED,
        flags,
        name,
        decl,
    };

    dynamic_array_append(&symbols, sym);
    return true;
}

bool add_unresolved_decl_symbol(AST_Declaration *decl, bool global)
{
    assert(decl);

    Symbol_Kind kind = Symbol_Kind::INVALID;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE: kind = Symbol_Kind::VAR; break;

        case AST_Declaration_Kind::FUNCTION: kind = Symbol_Kind::FUNC; break;

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: kind = Symbol_Kind::TYPE; break;
    }

    assert(kind != Symbol_Kind::INVALID);

    Symbol_Flags flags = SYM_FLAG_NONE;
    if (global) flags |= SYM_FLAG_GLOBAL;

    assert(decl->identifier && decl->identifier->kind == AST_Expression_Kind::IDENTIFIER);
    auto name = decl->identifier->identifier;

    return add_unresolved_symbol(kind, flags, name, decl);
}

Symbol *get_symbol(Atom name)
{
    for (u64 i = 0; i < symbols.count; i++) {
        if (symbols[i].name == name) {
            return &symbols[i];
        }
    }

    return nullptr;
}

bool name_resolve_decl_(AST_Declaration *decl)
{
    assert(decl);

    assert(decl->identifier && decl->identifier->kind == AST_Expression_Kind::IDENTIFIER);
    auto decl_sym = get_symbol(decl->identifier->identifier);
    assert(decl_sym && decl_sym->decl == decl);

    switch (decl_sym->state) {
        case Symbol_State::UNRESOLVED: decl_sym->state = Symbol_State::RESOLVING; break;
        case Symbol_State::RESOLVING: assert(false); // circ dep
        case Symbol_State::RESOLVED: return true;
    }

    bool result = true;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);
        case AST_Declaration_Kind::VARIABLE: assert(false);

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->constant_variable.type_spec;
            auto expr = decl->constant_variable.value;

            if (ts) name_resolve_ts(ts);
            if (expr) name_resolve_expr(expr);

            break;
        }

        case AST_Declaration_Kind::FUNCTION: assert(false);
        case AST_Declaration_Kind::STRUCT: assert(false);
        case AST_Declaration_Kind::UNION: assert(false);
    }



exit:
    decl_sym = get_symbol(decl->identifier->identifier);
    assert(decl_sym->state == Symbol_State::RESOLVING);

    if (result) {
        decl_sym->state = Symbol_State::RESOLVED;
        name_resolved_count += 1;
    } else {
        decl_sym->state = Symbol_State::UNRESOLVED;
    }

    return result;
}

bool name_resolve_stmt_(AST_Statement *stmt)
{
    assert(stmt);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);
        case AST_Statement_Kind::BLOCK: assert(false);
        case AST_Statement_Kind::DECLARATION: assert(false);
        case AST_Statement_Kind::ASSIGN: assert(false);
        case AST_Statement_Kind::CALL: assert(false);
        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);
        case AST_Statement_Kind::RETURN: assert(false);
        case AST_Statement_Kind::PRINT: assert(false);
    }   

    assert(false);
    return false;
}

bool name_resolve_expr_(AST_Expression *expr)
{
    assert(expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL: return true;

        case AST_Expression_Kind::IDENTIFIER: {
            Symbol *sym = get_symbol(expr->identifier);

            if (!sym) {
                resolve_error(expr, "Undefined symbol: '%s'", expr->identifier.data);
                return false;
            }

            if (sym->state == Symbol_State::RESOLVING) {
                assert(false); // circ dep
                return false;
            } else if (sym->state == Symbol_State::UNRESOLVED) {
                resolve_error(expr, "Unresolved symbol: '%s'", expr->identifier.data);
                return false;
            } else {
                assert(sym->state == Symbol_State::RESOLVED);
            }
            return true;
        }
                                              
        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);
        case AST_Expression_Kind::CALL: assert(false);
        case AST_Expression_Kind::UNARY: assert(false);
        case AST_Expression_Kind::BINARY: assert(false);
    }

    assert(false);
    return false;
}

bool name_resolve_ts_(AST_Type_Spec *ts)
{
    assert(ts);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);
        case AST_Type_Spec_Kind::NAME: assert(false);
        case AST_Type_Spec_Kind::POINTER: assert(false);
    }

    assert(false);
    return false;
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
