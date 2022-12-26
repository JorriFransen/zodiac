#pragma once

#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h" // for Source_Pos

namespace Zodiac
{

struct Allocator;
struct AST_Identifier;
struct AST_Declaration;
struct Scope;

enum class Symbol_Kind : u16
{
    INVALID,

    FUNC,

    VAR,
    CONST,
    PARAM,

    TYPE,
    MEMBER,
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
    Source_Pos pos;

    union
    {
        struct
        {
            Scope *parameter_scope;
            Scope *local_scope;
        } func;

        struct
        {
            Scope *scope;
        } aggregate;
    };
};

enum class Scope_Kind
{
    GLOBAL,
    FUNCTION_PARAMETER,
    FUNCTION_LOCAL,
    AGGREGATE,
};

struct Scope
{
    Scope_Kind kind;
    Scope *parent;
    Dynamic_Array<Symbol> symbols;
};

ZAPI Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent);

ZAPI Symbol *scope_get_symbol(Scope *scope, const Atom &name);
ZAPI Symbol *scope_get_symbol(Scope *scope, const AST_Identifier &ident);

ZAPI Symbol *scope_add_symbol(Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl);
ZAPI Symbol *scope_add_symbol(Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl, Source_Pos pos);

ZAPI Symbol *add_unresolved_symbol(Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);
ZAPI Symbol *add_unresolved_symbol(Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl, Source_Pos pos);

ZAPI Symbol *add_resolved_symbol(Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);
ZAPI Symbol *add_unresolved_decl_symbol(Scope *scope, AST_Declaration *decl, bool global);

}
