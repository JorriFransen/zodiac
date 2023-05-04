#include "error.h"

#include "containers/dynamic_array.h"
#include "util/asserts.h"
#include "zodiac_context.h"

namespace Zodiac
{

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos start, Source_Pos end, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = zodiac_report_error(context, kind, start, end, fmt, args);

    va_end(args);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos start, Source_Pos end, const char *fmt, va_list args)
{
    String message = string_format(&context->error_allocator, fmt, args);
    auto result = zodiac_report_error(context, kind, start, end, message);

    return result;
}

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos start, Source_Pos end, String message)
{
    assert(kind != Zodiac_Error_Kind::ZODIAC_ERROR_INVALID);

    Zodiac_Error err = {
        .kind = kind,
        .start = start,
        .end = end,
        .message = message,
    };

    s64 index = context->errors.count;
    dynamic_array_append(&context->errors, err);
    return (Error_Handle)index;
}
}
