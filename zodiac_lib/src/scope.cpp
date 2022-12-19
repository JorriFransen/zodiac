#include "scope.h"

#include "asserts.h"
#include "ast.h"
#include "atom.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "resolve.h"

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

Symbol *scope_add_symbol(Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(scope);
    assert(state != Symbol_State::RESOLVING);
    assert(decl || (flags & SYM_FLAG_BUILTIN));

    auto ex_sym = scope_get_symbol(scope, name);
    if (ex_sym) {
        assert(decl);
        report_redecl(ex_sym, name, decl->pos);
        return nullptr;
    }

    Symbol sym = { kind, state, flags, name, decl };

    dynamic_array_append(&scope->symbols, sym);

    return &scope->symbols[scope->symbols.count - 1];
}

Symbol *add_unresolved_symbol(Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(scope);
    return scope_add_symbol(scope, kind, Symbol_State::UNRESOLVED, flags, name, decl);
}

Symbol *add_resolved_symbol(Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl)
{
    assert(scope);
    return scope_add_symbol(scope, kind, Symbol_State::RESOLVED, flags, name, decl);
}

Symbol *add_unresolved_decl_symbol(Scope *scope, AST_Declaration *decl, bool global)
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

        case AST_Declaration_Kind::FUNCTION: {
            kind = Symbol_Kind::FUNC;

            parameter_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_PARAMETER, scope);
            local_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, parameter_scope);

            for (u64 i = 0; i < decl->function.params.count; i++) {
                auto param = decl->function.params[i];

                auto param_sym = add_unresolved_symbol(parameter_scope, Symbol_Kind::PARAM, SYM_FLAG_NONE, param.identifier.name, decl);
                if (!param_sym) {
                    return nullptr;
                }
            }
            break;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            kind = Symbol_Kind::TYPE;

            aggregate_scope = scope_new(&dynamic_allocator, Scope_Kind::AGGREGATE, scope);

            for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
                auto field = decl->aggregate.fields[i];
                auto mem_sym = add_unresolved_symbol(aggregate_scope, Symbol_Kind::MEMBER, SYM_FLAG_NONE, field.identifier.name, decl);
                if (!mem_sym) {
                    return nullptr;
                }
            }
            break;
        }
    }

    assert(kind != Symbol_Kind::INVALID);

    Symbol_Flags flags = SYM_FLAG_NONE;
    if (global) flags |= SYM_FLAG_GLOBAL;

    Symbol *result = add_unresolved_symbol(scope, kind, flags, decl->identifier.name, decl);

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

}
