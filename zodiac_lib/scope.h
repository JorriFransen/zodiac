#pragma once

#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "source_pos.h"

namespace Zodiac
{

struct AST_Declaration;
struct AST_File;
struct AST_Identifier;
struct Allocator;
struct Scope;
struct Type;
struct Zodiac_Context;

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
    UNRESOLVED = 1,

    RESOLVING  = 2,
    RESOLVED   = 3,

    TYPED      = 4,
};

typedef u32 Symbol_Flags;

enum Symbol_Flag : Symbol_Flags
{
    SYM_FLAG_NONE    = 0x00,
    SYM_FLAG_BUILTIN = 0x01,
};

struct Symbol
{
    Symbol_Kind kind;
    Symbol_State state;
    Symbol_Flags flags;

    Atom name;
    AST_Declaration *decl;
    Source_Range range;

    Type* builtin_type;

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
            Type *unfinished_struct_type;
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

    union {
        AST_Declaration *func_decl; // Only set in FUNCTION_PARAMETER scopes
        AST_File *file;
    };
};

ZAPI Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent);

ZAPI Symbol *scope_get_symbol(Scope *scope, const Atom &name);
ZAPI Symbol *scope_get_symbol(Scope *scope, const AST_Identifier &ident);

ZAPI Symbol *scope_get_symbol_direct(Scope *scope, const Atom &name, s64 *index);

ZAPI Symbol *scope_add_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl);
ZAPI Symbol *scope_add_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_State state, Symbol_Flags flags, Atom name, AST_Declaration *decl, Source_Range range);

ZAPI Symbol *add_unresolved_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);
ZAPI Symbol *add_unresolved_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl, Source_Range range);

ZAPI Symbol *add_typed_symbol(Zodiac_Context *ctx, Scope *scope, Symbol_Kind kind, Symbol_Flags flags, Atom name, AST_Declaration *decl);
ZAPI bool add_unresolved_decl_symbol(Zodiac_Context *ctx, Scope *scope, AST_Declaration *decl, bool global);

ZAPI AST_Declaration *enclosing_function(Scope *scope);
ZAPI AST_File *enclosing_file(Scope *scope);
}
