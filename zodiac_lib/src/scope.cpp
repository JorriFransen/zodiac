#include "scope.h"

#include "ast.h"
#include "atom.h"
#include "error.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "util/asserts.h"
#include "zodiac_context.h"

// (Complaining about String_Ref)
// IWYU pragma: no_include "util/zstring.h"

namespace Zodiac
{

Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent)
{
    assert(allocator);

    if (kind == Scope_Kind::GLOBAL) {
        assert_msg(!parent, "Global scope cannot have a parent scope");
    } else {
        assert_msg(parent, "Expected parent scope for non global scope");
    }

    Scope *result = alloc<Scope>(allocator);
    assert(result);

    result->kind = kind;
    result->parent = parent;
    result->func_decl = nullptr;
    dynamic_array_create(allocator, &result->symbols);

    return result;
}

Symbol *scope_get_symbol(Scope *scope, const Atom &name)
{
    for (u64 i = 0; i < scope->symbols.count; i++) {
        if (scope->symbols[i].name == name) {
            return &scope->symbols[i];
        }
    }

    while (scope->parent) {
        scope = scope->parent;

        for (u64 i = 0; i < scope->symbols.count; i++) {
            if (scope->symbols[i].name == name) {
                return &scope->symbols[i];
            }
        }
    }

    return nullptr;
}

Symbol *scope_get_symbol(Scope *scope, const AST_Identifier &ident)
{
    assert(scope);

    return scope_get_symbol(scope, ident.name);
}

Symbol *scope_get_symbol_direct(Scope *scope, const Atom &name, s64 *index)
{
    assert(scope);

    for (u64 i = 0; i < scope->symbols.count; i++) {
        if (scope->symbols[i].name == name) {
            if (index) *index = i;
            return &scope->symbols[i];
        }
    }

    if (index) *index = -1;
    return nullptr;
}

Symbol *scope_add_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(scope);

    Source_Range range = {};

    if (decl) {
        range = decl->range;
    } else {
        Source_Pos pos = { .name = "<builtin>", .line = 0, .index_in_line = 0, };
        range = { pos, pos };
    }

    return scope_add_symbol(ctx, scope, kind, state, flags, name, decl, range);
}

Symbol *scope_add_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl, Source_Range range)
{
    assert(ctx);
    assert(scope);
    assert(state != Symbol_State::RESOLVING);
    assert(decl || (flags & SYM_FLAG_BUILTIN));

    auto ex_sym = scope_get_symbol(scope, name);
    if (ex_sym) {
        assert(decl);
        report_redecl(ctx, ex_sym->range, name, decl->identifier.range);
        return nullptr;
    }

    Symbol sym = { kind, state, flags, name, decl, range };

    dynamic_array_append(&scope->symbols, sym);

    return &scope->symbols[scope->symbols.count - 1];
}

Symbol *add_unresolved_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    return add_unresolved_symbol(ctx, scope, kind, flags, name, decl, decl->range);
}

Symbol *add_unresolved_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl, Source_Range range)
{
    assert(scope && decl);
    return scope_add_symbol(ctx, scope, kind, Symbol_State::UNRESOLVED, flags, name, decl, range);
}

Symbol *add_typed_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(scope);

    if (!decl) assert(flags & SYM_FLAG_BUILTIN);

    return scope_add_symbol(ctx, scope, kind, Symbol_State::TYPED, flags, name, decl);
}

Symbol *add_unresolved_decl_symbol(Zodiac_Context *ctx, Scope *scope, AST_Declaration *decl, bool global)
{
    assert(scope && decl);

    Symbol_Kind kind = Symbol_Kind::INVALID;

    Scope *parameter_scope = nullptr;
    Scope *local_scope = nullptr;
    Scope *aggregate_scope = nullptr;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:          kind = Symbol_Kind::VAR; break;
        case AST_Declaration_Kind::CONSTANT_VARIABLE: kind = Symbol_Kind::CONST; break;

        case AST_Declaration_Kind::PARAMETER: assert(false); break;
        case AST_Declaration_Kind::FIELD: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {
            kind = Symbol_Kind::FUNC;

            parameter_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_PARAMETER, scope);
            local_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, parameter_scope);

            for (u64 i = 0; i < decl->function.params.count; i++) {
                auto param = decl->function.params[i];

                auto param_sym = add_unresolved_symbol(ctx, parameter_scope, Symbol_Kind::PARAM, SYM_FLAG_NONE, param->identifier.name, param, param->identifier.range);
                if (!param_sym) {
                    return nullptr;
                }
            }

            parameter_scope->func_decl = decl;
            break;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            kind = Symbol_Kind::TYPE;

            aggregate_scope = scope_new(&dynamic_allocator, Scope_Kind::AGGREGATE, scope);

            for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
                auto field = decl->aggregate.fields[i];
                auto mem_sym = add_unresolved_symbol(ctx, aggregate_scope, Symbol_Kind::MEMBER, SYM_FLAG_NONE, field->identifier.name, field, field->identifier.range);
                if (!mem_sym) {
                    return nullptr;
                }
            }
            break;
        }

        case AST_Declaration_Kind::RUN_DIRECTIVE: {
            assert(false);
            break;
        }
    }

    assert(kind != Symbol_Kind::INVALID);

    if (global) decl->flags |= AST_DECL_FLAG_GLOBAL;

    Symbol *result = add_unresolved_symbol(ctx, scope, kind, SYM_FLAG_NONE, decl->identifier.name, decl);
    if (!result) {
        return nullptr;
    }

    switch (result->kind) {
        case Symbol_Kind::INVALID: assert(false);

        case Symbol_Kind::VAR:
        case Symbol_Kind::CONST:
        case Symbol_Kind::PARAM:
        case Symbol_Kind::MEMBER: {
            assert(!parameter_scope&& !local_scope && !aggregate_scope);
            break;
        }

        case Symbol_Kind::FUNC: {
            assert(parameter_scope && local_scope);
            assert(local_scope->parent == parameter_scope);
            assert(parameter_scope->parent == scope);

            result->func.parameter_scope = parameter_scope;
            result->func.local_scope = local_scope;
            break;
        }

        case Symbol_Kind::TYPE: {
            assert(aggregate_scope);
            assert(aggregate_scope->parent == scope);

            result->aggregate.scope = aggregate_scope;
            break;
        }

    }

    return result;
}

AST_Declaration *enclosing_function(Scope *scope)
{
    assert(scope);
    assert(scope->kind != Scope_Kind::GLOBAL)

    Scope *current = scope;
    while (true) {
        if (current->kind == Scope_Kind::FUNCTION_PARAMETER) {
            auto decl = current->func_decl;
            assert(decl);
            assert(decl->kind == AST_Declaration_Kind::FUNCTION);
            return decl;
        }

        current = current->parent;
    }
}

}
