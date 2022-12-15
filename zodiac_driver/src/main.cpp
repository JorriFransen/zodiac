
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

    Dynamic_Array<Symbol *> dependencies;
};

struct Resolve_Error
{
    String message;
    Source_Pos pos;
    bool fatal;
};

Dynamic_Array<Symbol *> name_resolved_symbols;

Dynamic_Array<Resolve_Error> resolve_errors;
bool fatal_resolve_error = false;

bool name_resolve_decl_(AST_Declaration *decl, bool global);
bool name_resolve_stmt_(AST_Statement *stmt, Symbol *depender);
bool name_resolve_expr_(AST_Expression *expr, Symbol *depender);
bool name_resolve_ts_(AST_Type_Spec *ts);

#define name_resolve_decl(decl) {           \
    if (!name_resolve_decl_(decl, false)) { \
        return false;                       \
    }                                       \
}

#define name_resolve_stmt(stmt, depender) {        \
    if (!name_resolve_stmt_((stmt), (depender))) { \
        return false;                              \
    }                                              \
}

#define name_resolve_expr(expr, depender) {        \
    if (!name_resolve_expr_((expr), (depender))) { \
        return false;                              \
    }                                              \
}

#define name_resolve_ts(ts) {    \
    if (!name_resolve_ts_(ts)) { \
        return false;            \
    }                            \
}

Symbol *get_symbol(const Atom name);

bool add_symbol(Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);

Symbol *register_symbol(AST_Declaration *decl, bool global);

void register_dependency(Symbol *depender, Symbol *dependency);

bool has_dependency(AST_Declaration *a, AST_Declaration *b);
void report_circular_dep(AST_Declaration *decl);

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args);
void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...);

#define resolve_error(node, fmt, ...) resolve_error_((node), false, (fmt), ##__VA_ARGS__);

#define fatal_resolve_error(node, fmt, ...) {    \
    resolve_error_((node), true, (fmt), ##__VA_ARGS__); \
    fatal_resolve_error = true;                  \
}

void resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(ctx);

    auto at = &ctx->atoms;

    dynamic_array_create(&dynamic_allocator, &name_resolved_symbols);
    dynamic_array_create(&dynamic_allocator, &resolve_errors);

    add_symbol(SYM_TYPE, SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN, atom_s64, nullptr);
    add_symbol(SYM_TYPE, SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN, atom_r32, nullptr);
    add_symbol(SYM_TYPE, SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN, atom_String, nullptr);
    add_symbol(SYM_TYPE, SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN, atom_get(at, "null"), nullptr);

    for (u64 i = 0; i < file->declarations.count; i++) {

        auto decl = file->declarations[i];
        register_symbol(decl, true);
    }

    u64 name_resolved_count = name_resolved_symbols.count;
    bool progress = true;
    bool done = false;

    auto file_decls = dynamic_array_copy(&file->declarations, &dynamic_allocator);

    while (progress && !done && !fatal_resolve_error) {

        done = true;
        for (u64 i = 0; i < file_decls.count; i++) {
            
            auto decl = file_decls[i];

            if (decl) {
                if (name_resolve_decl_(decl, true)) {
                    
                    // Set the decl to null instead of removing it (don't want to do unordered removal)
                    file_decls[i] = nullptr;

                    if (has_dependency(decl, decl)) {
                        report_circular_dep(decl);
                        break;
                    }
                } else {
                    done = false;
                }
            }
        }

        progress = name_resolved_count < name_resolved_symbols.count;
        name_resolved_count = name_resolved_symbols.count;

        if (progress && !fatal_resolve_error) {
            resolve_errors.count = 0;
            temporary_allocator_reset(&ctx->resolve_error_allocator_state);
        }
    }

    for (u64 i = 0; i < resolve_errors.count; i++) {
        auto err = resolve_errors[i];

        if (!fatal_resolve_error) assert(err.fatal == false);

        bool print = fatal_resolve_error == err.fatal;

        if (print) {
            printf("%s:%llu:%llu: error: %s\n", err.pos.name.data, err.pos.line, err.pos.index_in_line, err.message.data);
        }
    }

    if (resolve_errors.count == 0) {
        for (u64 i = 0; i < name_resolved_symbols.count; i++) {
            // printf("%s\n", name_resolved_symbols[i].name.data);
            auto resolved_sym = name_resolved_symbols[i];

            if (resolved_sym->decl && (resolved_sym->flags & SYM_FLAG_GLOBAL)) {
                ast_print_declaration(resolved_sym->decl);
                printf("\n\n");
            }
        }
    }

    dynamic_array_free(&file_decls);
}

bool name_resolve_decl_(AST_Declaration *decl, bool global)
{
    assert(decl);

    assert(decl->identifier->kind == AST_Expression_Kind::IDENTIFIER);
    auto decl_name = decl->identifier->identifier;

    Symbol *sym = nullptr;

    if (global) {
        sym = get_symbol(decl_name);
        if (sym) {
            if (sym->decl != decl) {
                fatal_resolve_error(decl, "Redeclaration of symbol '%s'", decl_name.data);
                return false;
            }
            assert(((sym->flags & SYM_FLAG_GLOBAL) == SYM_FLAG_GLOBAL) == global);
        } else {
            assert_msg(false, "Expected symbol for global decl");
        }

    } else {
        
        // Non global
        sym = get_symbol(decl_name);
        if (sym && sym->decl != decl) {
            fatal_resolve_error(decl, "Redeclaration of symbol '%s'", decl_name.data);
            return false;
        }
        if (!sym) {
            sym = register_symbol(decl, global);
        }
    }

    assert(sym);

    Symbol_Flags sym_flags = SYM_FLAG_NONE;
    if (global) sym_flags |= SYM_FLAG_GLOBAL;

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {
            auto ts = decl->variable.type_spec;
            auto expr = decl->variable.value;

            if (ts) name_resolve_ts(ts);
            if (expr) name_resolve_expr(expr, sym);

            //TODO: HACK: Don't put this in the global symbol table!!!
            // add_symbol(SYM_VAR, sym_flags, decl_name, decl);
            return true;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->constant_variable.type_spec;
            auto expr = decl->constant_variable.value;

            if (ts) name_resolve_ts(ts);
            if (expr) name_resolve_expr(expr, sym);

            //TODO: HACK: Don't put this in the global symbol table!!!
            // add_symbol(SYM_VAR, sym_flags, decl_name, decl);
            return true;
        }

        case AST_Declaration_Kind::FUNCTION: {

            for (u64 i = 0; i < decl->function.params.count; i++) {
                auto param_field = decl->function.params[i];
                
                {
                    auto sym = get_symbol(param_field.name);
                    if (sym) {
                        if (sym->decl != decl) {
                            fatal_resolve_error(decl, "Redeclaration of symbol '%s'", decl_name.data);
                            return false;
                        }

                        assert(sym->kind == SYM_PARAM);
                        continue;
                    }
                }
                name_resolve_ts(param_field.type_spec);

                //TODO: HACK: Don't put this in the global symbol table!!!
                if (!add_symbol(SYM_PARAM, SYM_FLAG_NONE, param_field.name, decl)) {
                    assert(false);
                }
            }

            name_resolve_ts(decl->function.return_ts);

            for (u64 i = 0; i < decl->function.body.count; i++) {
                auto stmt = decl->function.body[i];

                name_resolve_stmt(stmt, sym);
            }

            // add_symbol(SYM_FUNC, sym_flags, decl_name, decl);
            return true;
        } 

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            // TODO: Start creating struct/union type and add resolved fields to it
            for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
                auto field = decl->aggregate.fields[i];
                name_resolve_ts(field.type_spec);
            }

            // add_symbol(SYM_TYPE, sym_flags, decl_name, decl);
            return true;
        }

    }

    assert(false);
    return false;
}

bool name_resolve_stmt_(AST_Statement *stmt, Symbol *depender)
{
    assert(stmt);

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false);
        case AST_Statement_Kind::BLOCK: assert(false);

        case AST_Statement_Kind::DECLARATION: {
            name_resolve_decl(stmt->decl.decl);
            return true;
        }

        case AST_Statement_Kind::ASSIGN: assert(false);
        case AST_Statement_Kind::CALL: assert(false);
        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                name_resolve_expr(stmt->return_stmt.value, depender);
            }
            return true;
        }

        case AST_Statement_Kind::PRINT: {
            name_resolve_expr(stmt->print_expr, depender);
            return true;
        }
    }

    assert(false);
    return false;
}

bool name_resolve_expr_(AST_Expression *expr, Symbol *depender)
{
    assert(expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL: return true;

        case AST_Expression_Kind::IDENTIFIER: {
            auto sym = get_symbol(expr->identifier);
            if (!sym) {
                resolve_error(expr, "Undefined symbol '%s'", expr->identifier.data);
                return false;
            }

            if (depender) {
                register_dependency(depender, sym);
            }

            return true;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {
            name_resolve_expr(expr->call.base, depender);

            for (u64 i = 0; i < expr->call.args.count; i++) {
                auto arg_expr = expr->call.args[i];
                name_resolve_expr(arg_expr, depender);
            }

            return true;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            name_resolve_expr(expr->binary.lhs, depender);
            name_resolve_expr(expr->binary.rhs, depender);

            return true;
        }
    }

    assert(false);
    return false;
}

bool name_resolve_ts_(AST_Type_Spec *ts)
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
            return true;
        }

        case AST_Type_Spec_Kind::POINTER: {
            name_resolve_ts(ts->base);
            return true;
        }
    }
    
    assert(false);
    return false;
}

Symbol *get_symbol(const Atom name)
{
    for (u64 i = 0; i < name_resolved_symbols.count; i++) {
        auto sym = name_resolved_symbols[i];
        if (sym->name == name) {
            return sym;
        }
    }

    return nullptr;
}

bool add_symbol(Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(kind != Symbol_Kind::SYM_INVALID);

    if (get_symbol(name)) {
        fatal_resolve_error(decl, "Redeclaration of symbol '%s'", name.data);
        return false;
    }

    Symbol *result = alloc<Symbol>(&dynamic_allocator);
    result->kind = kind;
    result->flags = flags;
    result->name = name;
    result->decl = decl;
    dynamic_array_create(&dynamic_allocator, &result->dependencies, 0);


    dynamic_array_append(&name_resolved_symbols, result);
    return true;
}

Symbol *register_symbol(AST_Declaration *decl, bool global)
{
    assert(decl);
    assert(decl->identifier);
    assert(decl->identifier->kind == AST_Expression_Kind::IDENTIFIER);

    Symbol_Kind kind = SYM_INVALID;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE: kind = SYM_VAR; break;

        case AST_Declaration_Kind::FUNCTION: kind = SYM_FUNC; break;

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: kind = SYM_TYPE; break;
    }

    assert(kind != SYM_INVALID);

    Symbol_Flags flags = SYM_FLAG_NONE;
    if (global) flags |= SYM_FLAG_GLOBAL;

    auto decl_name = decl->identifier->identifier;

    bool result = add_symbol(kind, flags, decl_name, decl);
    assert(result);

    auto sym = get_symbol(decl_name);
    assert(sym->decl == decl);
    return sym;
}

void register_dependency(Symbol *depender, Symbol *dependency)
{
    assert(depender && dependency);

    bool found = false;

    for (u64 i = 0; i < depender->dependencies.count; i++) {
        if (depender->dependencies[i] == dependency) {
            found = true;
            break;
        }
    }

    if (!found) {
        dynamic_array_append(&depender->dependencies, dependency);
    }
}

bool has_dependency(Symbol *a, Symbol *b)
{
    assert(a && b);

    for (u64 i = 0; i < a->dependencies.count; i++) {

        if (a->dependencies[i] == b) return true;

        if (has_dependency(a->dependencies[i], b)) {
            return true;
        }
    }

    return false;
}

bool has_dependency(AST_Declaration *a, AST_Declaration *b)
{
    assert(a && b);
    assert(a->identifier && a->identifier->kind == AST_Expression_Kind::IDENTIFIER);
    assert(b->identifier && b->identifier->kind == AST_Expression_Kind::IDENTIFIER);

    auto sym_a = get_symbol(a->identifier->identifier);
    auto sym_b = get_symbol(b->identifier->identifier);
    assert(sym_a && sym_b);

    return has_dependency(sym_a, sym_b);
}

void report_circular_dep(Symbol *current, Symbol *root)
{
    assert(current && root);

    for (u64 i = 0; i < current->dependencies.count; i++) {
        if (has_dependency(current->dependencies[i], root)) {
            auto dep = current->dependencies[i];
            fatal_resolve_error(current->decl, "'%s' depends on '%s'", current->name.data, dep->name.data);
            if (dep != root) report_circular_dep(dep, root);
            break;
        }
    }
}

void report_circular_dep(AST_Declaration *root_decl)
{
    assert(root_decl);
    assert(root_decl->identifier && root_decl->identifier->kind == AST_Expression_Kind::IDENTIFIER);

    auto sym = get_symbol(root_decl->identifier->identifier);
    assert(sym);

    fatal_resolve_error(root_decl, "Cyclic dependency detected");
    report_circular_dep(sym, sym);
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
