#pragma once

#include <stdarg.h>

#include "asserts.h"
#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h"
#include "zstring.h"

namespace Zodiac
{

struct Zodiac_Context;
struct AST_Declaration;
struct AST_Expression;
struct AST_Statement;
struct AST_Type_Spec;

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
};

struct Resolve_Error
{
    String message;
    Source_Pos pos;
    bool fatal;
};

ZAPI extern Dynamic_Array<Symbol> symbols;
ZAPI extern u64 name_resolved_count;

ZAPI extern Dynamic_Array<Resolve_Error> resolve_errors;
ZAPI extern bool fatal_resolve_error;

ZAPI extern Zodiac_Context *ctx;

ZAPI void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args);
ZAPI void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, ...);
ZAPI void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...);
ZAPI void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...);
ZAPI void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...);
ZAPI void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...);

#define resolve_error(node, fmt, ...) resolve_error_((node), false, fmt, ##__VA_ARGS__);

#define fatal_resolve_error(node, fmt, ...) {           \
    fatal_resolve_error = true;                         \
    resolve_error_((node), true, (fmt), ##__VA_ARGS__); \
}

#define report_redecl(old_sym, new_ident) {                                                       \
    fatal_resolve_error((new_ident).pos, "Redeclaration of symbol: '%s'", (new_ident).name.data); \
    assert((old_sym)->decl);                                                                      \
    fatal_resolve_error((old_sym)->decl->pos, "<---- Previous declaration was here");             \
}

}
