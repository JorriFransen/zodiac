#include "error.h"

#include "ast.h"
#include "containers/dynamic_array.h"
#include "util/asserts.h"
#include "zodiac_context.h"

namespace Zodiac
{

u64 resolve_error_count(Zodiac_Context *ctx)
{
    u64 result = 0;

    for (u64 i = 0; i < ctx->errors.count; i++) {
        if (ctx->errors[i].kind == ZODIAC_RESOLVE_ERROR) {
            result += 1;
        }
    }

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Range sr, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, sr, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Range sr, const char *fmt, va_list args)
{
    String message = string_format_va_list(&context->error_allocator, fmt, args);
    auto result = zodiac_report_error(context, kind, sr, message);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Range sr, String message)
{
    assert(kind != Zodiac_Error_Kind::ZODIAC_ERROR_INVALID);

    Zodiac_Error err = {
        .kind = kind,
        .source_range = sr,
        .message = message,
    };

    s64 index = context->errors.count;
    dynamic_array_append(&context->errors, err);
    return (Error_Handle)index;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos pos, const char *fmt, va_list args)
{
    return zodiac_report_error(context, kind, {pos, pos}, fmt, args);
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos pos, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    Source_Range sr = { pos, pos };
    auto result = zodiac_report_error(context, kind, sr, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Expression *expr, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, expr->sr, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Statement *stmt, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, stmt->sr, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Declaration *decl, const char *fmt, ...)
{

    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, decl->sr, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Type_Spec *ts, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, ts->sr, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, AST_Directive *directive, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, directive->sr, fmt, args);

    va_end(args);

    return result;
}

}
