#pragma once

#include "ast.h"

namespace Zodiac
{

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

}
