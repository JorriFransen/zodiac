#pragma once

#include <defines.h>

namespace Zodiac
{

enum class Log_Level
{
    FATAL = 0,
    ERROR = 1,
    WARN  = 2,
    INFO  = 3,
    DEBUG = 4,
    TRACE = 5,
};

ZAPI void log_message(Log_Level level, const char *fmt, ...);

}
