
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

    AST_Identifier ident;
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

bool add_unresolved_symbol(Symbol_Kind kind, Symbol_Flags flags, AST_Identifier ident, AST_Declaration *decl);
bool add_resolved_symbol(Symbol_Kind kind, Symbol_Flags flags, AST_Identifier ident, AST_Declaration *decl);
bool add_unresolved_decl_symbol(AST_Declaration *decl, bool global);

Symbol *get_symbol(const AST_Identifier &ident);
Symbol *get_symbol(const Atom &name);

bool name_resolve_decl_(AST_Declaration *decl, bool global);
bool name_resolve_stmt_(AST_Statement *stmt);
bool name_resolve_expr_(AST_Expression *expr);
bool name_resolve_ts_(AST_Type_Spec *ts);

#define name_resolve_decl(decl, glob) {        \
    if (!name_resolve_decl_((decl), (glob))) { \
        result = false;                        \
        goto exit;                             \
    }                                          \
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
void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...);
void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...);

#define resolve_error(node, fmt, ...) resolve_error_((node), false, fmt, ##__VA_ARGS__);

#define fatal_resolve_error(node, fmt, ...) {           \
    fatal_resolve_error = true;                         \
    resolve_error_((node), true, (fmt), ##__VA_ARGS__); \
}

#define report_redecl(old_sym, new_ident) {\
    fatal_resolve_error((new_ident).pos, "Redeclaration of symbol: '%s'", (new_ident).name.data); \
    fatal_resolve_error((old_sym)->ident.pos, "<---- Previous declaration was here"); \
}

#define add_builtin_symbol(kind, atom) {                                                         \
    Source_Pos pos = { "<builtin>", 0, 0 };                                                      \
    add_resolved_symbol((kind), (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), { (atom), pos }, nullptr); \
}

void resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(ctx);

    auto at = &ctx->atoms;

    dynamic_array_create(&dynamic_allocator, &symbols);
    dynamic_array_create(&dynamic_allocator, &resolve_errors);

    Symbol_Flags builtin_global_flags = SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN;

    add_builtin_symbol(Symbol_Kind::TYPE, atom_s64);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_r32);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_String);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_get(at, "null"));

    auto decls_to_resolve = dynamic_array_copy(&file->declarations, &dynamic_allocator);

    // Add global symbols
    for (u64 i = 0; i < decls_to_resolve.count; i++) {
        auto decl = decls_to_resolve[i];
        add_unresolved_decl_symbol(decl, true);
    }

    bool progress = true;
    bool done = false;

    while (progress && !done && !fatal_resolve_error) {

        auto last_name_resolved_count = name_resolved_count;

        done = true;
        for (u64 i = 0; i < decls_to_resolve.count; i++) {
            
            AST_Declaration *decl = decls_to_resolve[i];

            if (decl) {
                if (name_resolve_decl_(decl, true)) {
                    // Set the decl to null instead of removing it, don't want to do unordered removal!
                    decls_to_resolve[i] = nullptr;

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

    dynamic_array_free(&decls_to_resolve);

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

bool add_unresolved_symbol(Symbol_Kind kind, Symbol_Flags flags, AST_Identifier ident, AST_Declaration *decl)
{
    assert(kind != Symbol_Kind::INVALID);
    assert(decl);

    auto ex_sym = get_symbol(ident);
    if (ex_sym) {
        report_redecl(ex_sym, ident);
        return false;
    }

    Symbol sym = {
        kind,
        Symbol_State::UNRESOLVED,
        flags,
        ident,
        decl,
    };

    dynamic_array_append(&symbols, sym);
    return true;
}

bool add_resolved_symbol(Symbol_Kind kind, Symbol_Flags flags, AST_Identifier ident, AST_Declaration *decl)
{
    assert(kind != Symbol_Kind::INVALID);
    assert(decl || (flags & SYM_FLAG_BUILTIN));

    auto ex_sym = get_symbol(ident);
    if (ex_sym) {
        report_redecl(ex_sym, ident);
        return false;
    }

    Symbol sym = {
        kind,
        Symbol_State::RESOLVED,
        flags,
        ident,
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

        case AST_Declaration_Kind::FUNCTION: {
            kind = Symbol_Kind::FUNC;

            for (u64 i = 0; i < decl->function.params.count; i++) {
                auto param = decl->function.params[i];
                if (!add_unresolved_symbol(Symbol_Kind::PARAM, SYM_FLAG_NONE, param.identifier, decl)) {
                    return false;
                }
            }
            break;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: kind = Symbol_Kind::TYPE; break;
    }

    assert(kind != Symbol_Kind::INVALID);

    Symbol_Flags flags = SYM_FLAG_NONE;
    if (global) flags |= SYM_FLAG_GLOBAL;

    return add_unresolved_symbol(kind, flags, decl->identifier, decl);
}

Symbol *get_symbol(const AST_Identifier &ident)
{
    return get_symbol(ident.name);
}

Symbol *get_symbol(const Atom &name)
{
    for (u64 i = 0; i < symbols.count; i++) {
        if (symbols[i].ident.name == name) {
            return &symbols[i];
        }
    }

    return nullptr;
}

bool name_resolve_decl_(AST_Declaration *decl, bool global)
{
    assert(decl);

    auto decl_sym = get_symbol(decl->identifier.name);
    if (decl_sym && decl_sym->decl != decl) {
        report_redecl(decl_sym, decl->identifier);
        return false;
    }

    if (global) {
        assert_msg(decl_sym, "Global symbol should have been registered already");
    } else if (!decl_sym) {
        // First time local symbol is encountered 
        add_unresolved_decl_symbol(decl, global);
        decl_sym = get_symbol(decl->identifier);
    }
    assert(decl_sym && decl_sym->decl == decl);

    switch (decl_sym->state) {
        case Symbol_State::UNRESOLVED: decl_sym->state = Symbol_State::RESOLVING; break;
        case Symbol_State::RESOLVING: assert(false); // circ dep
        case Symbol_State::RESOLVED: return true;
    }

    bool result = true;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->constant_variable.type_spec;
            auto expr = decl->constant_variable.value;

            if (ts) name_resolve_ts(ts);
            if (expr) name_resolve_expr(expr);

            break;
        }

        case AST_Declaration_Kind::FUNCTION: {
            for (u64 i = 0; i < decl->function.params.count; i++) {
                auto param = decl->function.params[i];
                auto param_sym = get_symbol(param.identifier);
                assert(param_sym);

                switch (param_sym->state) {
                    case Symbol_State::UNRESOLVED: param_sym->state = Symbol_State::RESOLVING; break;
                    case Symbol_State::RESOLVING: assert(false); // Circular dependency
                    case Symbol_State::RESOLVED: continue;
                }

                name_resolve_ts(param.type_spec);
                param_sym->state = Symbol_State::RESOLVED;
            }

            auto return_ts = decl->function.return_ts;
            if (return_ts) name_resolve_ts(return_ts);

            for (u64 i = 0; i < decl->function.body.count; i++) {
                auto stmt = decl->function.body[i];

                name_resolve_stmt(stmt);
            }

            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false);
        case AST_Declaration_Kind::UNION: assert(false);
    }



exit:
    decl_sym = get_symbol(decl->identifier.name);
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

    bool result = true;

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);
        case AST_Statement_Kind::BLOCK: assert(false);

        case AST_Statement_Kind::DECLARATION: {
            name_resolve_decl(stmt->decl.decl, false);
            break;
        }

        case AST_Statement_Kind::ASSIGN: assert(false);
        case AST_Statement_Kind::CALL: assert(false);
        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            auto value = stmt->return_stmt.value;
            if (value) name_resolve_expr(value);
            break;
        }

        case AST_Statement_Kind::PRINT: {
            name_resolve_expr(stmt->print_expr);    
            break;
        }
    }   

exit:
    return result;
}

bool name_resolve_expr_(AST_Expression *expr)
{
    assert(expr);

    bool result = true;

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL: return true;

        case AST_Expression_Kind::IDENTIFIER: {
            Symbol *sym = get_symbol(expr->identifier.name);

            if (!sym) {
                resolve_error(expr, "Undefined symbol: '%s'", expr->identifier.name.data);
                return false;
            }

            if (sym->state == Symbol_State::RESOLVING) {

                fatal_resolve_error(expr, "Circular dependency detected");
                result = false;
                break;

            } else if (sym->state == Symbol_State::UNRESOLVED) {

                bool global = sym->flags & SYM_FLAG_GLOBAL;
                name_resolve_decl(sym->decl, global);

            } else {
                assert(sym->state == Symbol_State::RESOLVED);
            }

            break;
        }
                                              
        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {

            auto base = expr->call.base;
            name_resolve_expr(base);
            
            // TODO: Support more complex base expressions
            assert(base->kind == AST_Expression_Kind::IDENTIFIER);
            auto base_sym = get_symbol(base->identifier);
            assert(base_sym);

            if (base_sym->kind != Symbol_Kind::FUNC) {
                assert(base->kind == AST_Expression_Kind::IDENTIFIER);
                fatal_resolve_error(base, "Call expression base is not a function: '%s'", base->identifier.name.data);
                return false;
            }

            for (u64 i = 0; i < expr->call.args.count; i++) {
                auto arg = expr->call.args[i];

                name_resolve_expr(arg);
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

exit:
    return result;
}

bool name_resolve_ts_(AST_Type_Spec *ts)
{
    assert(ts);

    bool result = true;

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            auto sym = get_symbol(ts->identifier); 
            if (!sym) {
                resolve_error(ts, "Undeclared symbol: '%s'", ts->identifier.name.data);
                return false;
            }

            switch (sym->state) {
                case Symbol_State::UNRESOLVED: assert(false);
                case Symbol_State::RESOLVING: assert(false);
                case Symbol_State::RESOLVED: return true;
            }
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            name_resolve_ts(ts->base); 
            break;
        }
    }

exit:
    return result;
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args)
{
    Resolve_Error err;

    err.message = string_format(&ctx->resolve_error_allocator, fmt, args);
    err.pos = pos;
    err.fatal = fatal;

    dynamic_array_append(&resolve_errors, err);
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(pos, fatal, fmt, args);
    va_end(args);
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
