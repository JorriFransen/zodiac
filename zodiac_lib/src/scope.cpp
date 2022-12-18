#include "scope.h"

#include "asserts.h"
#include "ast.h"
#include "atom.h"
#include "memory/allocator.h"
#include "resolve.h"

namespace Zodiac
{

Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent)
{
    assert(allocator);
    assert(kind != Scope_Kind::INVALID);

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
        if (scope->symbols[i]->name == name) {
            return scope->symbols[i];
        }
    }

    while (scope->parent) {
        scope = scope->parent;

        for (u64 i = 0; i < scope->symbols.count; i++) {
            if (scope->symbols[i]->name == name) {
                return scope->symbols[i];
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

}
