#include "resolve_error.h"

#include "ast.h"
#include "zodiac_context.h"

namespace Zodiac
{

Zodiac_Context *ctx;
Dynamic_Array<Resolve_Error> resolve_errors;
bool fatal_resolve_error;

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args)
{
    Resolve_Error err;

    err.message = string_format(&ctx->error_allocator, fmt, args);
    err.pos = pos;
    err.fatal = fatal;

    dynamic_array_append(&resolve_errors, err);
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(decl->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(stmt->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(expr->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(ts->pos, fatal, fmt, args);
    va_end(args);
}
}
