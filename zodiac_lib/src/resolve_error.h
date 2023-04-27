#pragma once

#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h"
#include "util/zstring.h"

#include <stdarg.h>

namespace Zodiac
{

struct AST_Declaration;
struct AST_Expression;
struct AST_Statement;
struct AST_Type_Spec;
struct Zodiac_Context;


struct Resolve_Error
{
    String message;
    Source_Pos pos;
    bool fatal;
};

ZAPI extern Zodiac_Context *ctx;
ZAPI extern Dynamic_Array<Resolve_Error> resolve_errors;
ZAPI extern bool fatal_resolve_error;

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

#define report_redecl(old_pos, name, new_pos) {                                    \
    resolve_error_((new_pos), true, "Redeclaration of symbol: '%s'", (name).data); \
    fatal_resolve_error((old_pos), "<---- Previous declaration was here");         \
}

}
