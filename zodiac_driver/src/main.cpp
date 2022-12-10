
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

    ast_print_file(file);

    resolve_test(&c, file);

    free(&dynamic_allocator, stream.data);

    return 0;
}

enum Symbol_Kind
{
    SYM_INVALID,

    SYM_FUNC,

    SYM_VAR,
    SYM_PARAM,

    SYM_TYPE,
};

struct Symbol
{
    Symbol_Kind kind;
    Atom name;
    AST_Declaration *decl;    
};

struct Resolve_Error
{
    String message;
    Source_Pos pos;
};

Dynamic_Array<Symbol> name_resolved_symbols;

Dynamic_Array<Resolve_Error> resolve_errors;

void name_resolve_decl(AST_Declaration *decl);
void name_resolve_stmt(AST_Statement *stmt);
void name_resolve_expr(AST_Expression *expr);
void name_resolve_ts(AST_Type_Spec *ts);

Symbol *get_symbol(const Atom name);
void add_symbol(Symbol_Kind kind, Atom name, AST_Declaration *decl);


void resolve_error(Source_Pos pos, const String_Ref fmt, va_list args);
void resolve_error(AST_Declaration *decl, const String_Ref fmt, ...);
void resolve_error(AST_Statement *stmt, const String_Ref fmt, ...);
void resolve_error(AST_Expression *expr, const String_Ref fmt, ...);
void resolve_error(AST_Type_Spec *ts, const String_Ref fmt, ...);

void resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(ctx);

    auto at = &ctx->atoms;

    dynamic_array_create(&dynamic_allocator, &name_resolved_symbols);
    dynamic_array_create(&dynamic_allocator, &resolve_errors);

    add_symbol(SYM_TYPE, atom_s64, nullptr);
    add_symbol(SYM_TYPE, atom_r32, nullptr);
    add_symbol(SYM_TYPE, atom_String, nullptr);
    add_symbol(SYM_TYPE, atom_get(at, "null"), nullptr);

    for (u64 i = 0; i < file->declarations.count; i++) {
        
        auto decl = file->declarations[i];

        if (decl) {
            name_resolve_decl(decl);
        }
    }

    for (u64 i = 0; i < resolve_errors.count; i++) {
        auto err = resolve_errors[i];
        printf("%s:%llu:%llu: error: %s\n", err.pos.name.data, err.pos.line, err.pos.index_in_line, err.message.data);
    }
}

void name_resolve_decl(AST_Declaration *decl)
{
    assert(decl);

    assert(decl->identifier->kind == AST_Expression_Kind::IDENTIFIER);
    auto decl_name = decl->identifier->identifier;

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {
            auto ts = decl->variable.type_spec;
            auto expr = decl->variable.value;

            if (ts) name_resolve_ts(ts);
            if (expr) name_resolve_expr(expr);

            //TODO: HACK: Don't put this in the global symbol table!!!
            add_symbol(SYM_VAR, decl_name, decl);
            break;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->constant_variable.type_spec;
            auto expr = decl->constant_variable.value;

            if (ts) name_resolve_ts(ts);
            if (expr) name_resolve_expr(expr);

            //TODO: HACK: Don't put this in the global symbol table!!!
            add_symbol(SYM_VAR, decl_name, decl);
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            for (u64 i = 0; i < decl->function.params.count; i++) {
                auto param_field = decl->function.params[i];
                name_resolve_ts(param_field.type_spec);

                //TODO: HACK: Don't put this in the global symbol table!!!
                add_symbol(SYM_PARAM, param_field.name, decl);
            }

            name_resolve_ts(decl->function.return_ts);

            for (u64 i = 0; i < decl->function.body.count; i++) {
                auto stmt = decl->function.body[i];

                name_resolve_stmt(stmt);
            }

            add_symbol(SYM_FUNC, decl_name, decl);
            break;
        } 

        case AST_Declaration_Kind::STRUCT: {
            // TODO: Start creating struct type and add resolved fields to it
            for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
                auto field = decl->aggregate.fields[i];
                name_resolve_ts(field.type_spec);
            }

            add_symbol(SYM_TYPE, decl_name, decl);
            break;
        }

        case AST_Declaration_Kind::UNION: assert(false);
            break;
    }
}

void name_resolve_stmt(AST_Statement *stmt)
{
    assert(stmt);

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false);
        case AST_Statement_Kind::BLOCK: assert(false);

        case AST_Statement_Kind::DECLARATION: {
            name_resolve_decl(stmt->decl.decl);
            break;
        }

        case AST_Statement_Kind::ASSIGN: assert(false);
        case AST_Statement_Kind::CALL: assert(false);
        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                name_resolve_expr(stmt->return_stmt.value);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            name_resolve_expr(stmt->print_expr);
            break;
        }
    }
}

void name_resolve_expr(AST_Expression *expr)
{
    assert(expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL: break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto sym = get_symbol(expr->identifier);
            if (!sym) {
                resolve_error(expr, "Undefined symbol '%s'", expr->identifier.data);
            }
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {
            name_resolve_expr(expr->call.base);

            for (u64 i = 0; i < expr->call.args.count; i++) {
                auto arg_expr = expr->call.args[i];
                name_resolve_expr(arg_expr);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            name_resolve_expr(expr->binary.lhs);
            name_resolve_expr(expr->binary.rhs);
            break;
        }
    }
}

void name_resolve_ts(AST_Type_Spec *ts)
{
    assert(ts);

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            auto sym = get_symbol(ts->name);
            if (!sym) {
                resolve_error(ts, "Undefined symbol '%s'", ts->name.data);
            } else if (sym->kind != SYM_TYPE) {
                resolve_error(ts, "Not a type '%s'", ts->name.data);
            }
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            name_resolve_ts(ts->base);
            break;
        }
    }
}

Symbol *get_symbol(const Atom name)
{
    for (u64 i = 0; i < name_resolved_symbols.count; i++) {
        auto sym = &name_resolved_symbols[i];
        if (sym->name == name) {
            return sym;
        }
    }

    return nullptr;
}

void add_symbol(Symbol_Kind kind, Atom name, AST_Declaration *decl)
{
    assert(kind != Symbol_Kind::SYM_INVALID);

    if (get_symbol(name)) {
        resolve_error(decl, "Redeclaration of symbol '%s'", name);
    }
    dynamic_array_append(&name_resolved_symbols, { kind, name, decl });
}

void resolve_error(Source_Pos pos, const String_Ref fmt, va_list args)
{
    Resolve_Error err;

    err.message = string_format(&ctx->resolve_error_allocator, fmt, args);
    err.pos = pos;

    dynamic_array_append(&resolve_errors, err);
}

void resolve_error(AST_Declaration *decl, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error(decl->pos, fmt, args);
    va_end(args);
}

void resolve_error(AST_Statement *stmt, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error(stmt->pos, fmt, args);
    va_end(args);
}

void resolve_error(AST_Expression *expr, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error(expr->pos, fmt, args);
    va_end(args);
}

void resolve_error(AST_Type_Spec *ts, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error(ts->pos, fmt, args);
    va_end(args);
}
