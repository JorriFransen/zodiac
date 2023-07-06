#pragma once

#include <stdarg.h>

#include "defines.h"
#include "source_pos.h"
#include "util/zstring.h"

namespace Zodiac {

struct Zodiac_Context;

struct AST_Expression;
struct AST_Statement;
struct AST_Declaration;
struct AST_Type_Spec;

typedef s64 Error_Handle;

enum Zodiac_Error_Kind
{
    ZODIAC_ERROR_INVALID,
    ZODIAC_LEX_ERROR,
    ZODIAC_PARSE_ERROR,
    ZODIAC_RESOLVE_ERROR,
    ZODIAC_BC_CONVERSION_ERROR,
    ZODIAC_BC_VALIDATION_ERROR,

};

struct Zodiac_Error
{
    Zodiac_Error_Kind kind = ZODIAC_ERROR_INVALID;

    Source_Range source_range;

    bool fatal;

    String message = {};
};

ZAPI u64 resolve_error_count(Zodiac_Context *ctx);

ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Range sr, const char *fmt, ...);
ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Range sr, const char *fmt, va_list args);
ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Range sr, String message);

ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos pos, const char *fmt, ...);
ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Expression *expr, const char *fmt, ...);
ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Statement *stmt, const char *fmt, ...);
ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Declaration *decl, const char *fmt, ...);
ZAPI Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Type_Spec *ts, const char *fmt, ...);

#define resolve_error(ctx, node, fmt, ...) zodiac_report_error(ctx, ZODIAC_RESOLVE_ERROR, node, (fmt), ##__VA_ARGS__);

#define fatal_resolve_error(ctx, node, fmt, ...) {             \
    ctx->fatal_resolve_error = true;                           \
    auto handle = zodiac_report_error(ctx, ZODIAC_RESOLVE_ERROR, node, (fmt), ##__VA_ARGS__); \
    ctx->errors[handle].fatal = true; \
}

#define report_redecl(ctx, old_pos, name, new_pos) { \
    auto handle = zodiac_report_error(ctx, ZODIAC_RESOLVE_ERROR, (new_pos), "Redeclaration of symbol: '%s'", (name).data); \
    ctx->errors[handle].fatal = true; \
    fatal_resolve_error(ctx, (old_pos), "<---- Previous declaration was here");         \
}

}
