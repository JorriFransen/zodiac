#pragma once

#include <stdarg.h>

#include "asserts.h"
#include "atom.h" // IWYU pragma: keep
#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h"
#include "scope.h"
#include "zstring.h"

namespace Zodiac
{

struct AST_Declaration;
struct AST_Expression;
struct AST_File;
struct AST_Statement;
struct AST_Type_Spec;
struct Zodiac_Context;

struct Resolve_Error
{
    String message;
    Source_Pos pos;
    bool fatal;
};

struct Statement_Scope
{
    AST_Statement *stmt;
    Scope *scope;
};

ZAPI extern Scope *global_scope;
ZAPI extern Scope *current_scope;

ZAPI extern Dynamic_Array<Statement_Scope> statement_scopes;

ZAPI extern u64 name_resolved_count;

ZAPI extern Dynamic_Array<Resolve_Error> resolve_errors;
ZAPI extern bool fatal_resolve_error;

ZAPI extern Zodiac_Context *ctx;

ZAPI void resolve_test(Zodiac_Context *ctx, AST_File *file);

ZAPI bool name_resolve_decl_(AST_Declaration *decl, bool global);
ZAPI bool name_resolve_stmt_(AST_Statement *stmt);
ZAPI bool name_resolve_expr_(AST_Expression *expr);
ZAPI bool name_resolve_ts_(AST_Type_Spec *ts);

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

ZAPI bool is_lvalue_expr(AST_Expression *expr);
ZAPI bool is_const_expr(AST_Expression *expr);


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

#define report_redecl(old_sym, name, npos) {                                           \
    resolve_error_((npos), true, "Redeclaration of symbol: '%s'", (name).data); \
    assert((old_sym)->decl);                                                          \
    fatal_resolve_error((old_sym)->decl->pos, "<---- Previous declaration was here"); \
}

}
