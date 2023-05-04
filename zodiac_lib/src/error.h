#pragma once

#include "source_pos.h"

namespace Zodiac {

struct Zodiac_Context;

enum Zodiac_Error_Kind
{
    ZODIAC_ERROR_INVALID,
    ZODIAC_LEX_ERROR,
    ZODIAC_PARSE_ERROR,

};

struct Zodiac_Error
{
    Zodiac_Error_Kind kind = ZODIAC_ERROR_INVALID;

    Source_Pos start = {};
    Source_Pos end = {};

    String message = {};
};

typedef s64 Error_Handle;

Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos start, Source_Pos end, const char *fmt, ...);
Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos start, Source_Pos end, const char *fmt, va_list args);
Error_Handle zodiac_report_error(Zodiac_Context *context, Zodiac_Error_Kind kind, Source_Pos start, Source_Pos end, String message);

}
