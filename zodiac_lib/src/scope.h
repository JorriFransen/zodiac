#pragma once

#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct Allocator;
struct AST_Identifier;
struct Atom;
struct Symbol;

enum class Scope_Kind
{
    INVALID,

    GLOBAL,
    FUNCTION_PARAMETER,
    FUNCTION_LOCAL,
    AGGREGATE,
};

struct Scope
{
    Scope_Kind kind;
    Scope *parent;
    Dynamic_Array<Symbol *> symbols;
};

ZAPI Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent);

ZAPI Symbol *scope_get_symbol(Scope *scope, const Atom &name);
ZAPI Symbol *scope_get_symbol(Scope *scope, const AST_Identifier &ident);

}
