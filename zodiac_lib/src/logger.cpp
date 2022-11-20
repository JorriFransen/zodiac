#include "logger.h"

#include <platform/platform.h>
#include <zstring.h>

namespace Zodiac
{

void log_message(Log_Level log_level, const char *fmt, ...)
{
    i64 level_index = (i64)log_level;
    assert(level_index >= 0 && log_level <= Log_Level::TRACE);

    const char *level_strings[6] = { "[FATAL]: ", "[ERROR]: ", "[WARN]: ", "[INFO]: ", "[DEBUG]: ", "[TRACE]: "};
    const Platform_Console_Color level_colors[6] = { Platform_Console_Color::Red, Platform_Console_Color::Red, Platform_Console_Color::Yellow, Platform_Console_Color::Green, Platform_Console_Color::Blue, Platform_Console_Color::Grey };

    char out_message[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);

    auto out_length = string_format(out_message, fmt, args);

    va_end(args);

    if (out_length + 1 > ZSTRING_FORMAT_STACK_BUFFER_SIZE) {
        assert(false && "Formatted log message does not fit in stack buffer");
        return;
    }

    out_length = string_format(out_message, "%s%s\n", level_strings[level_index], out_message);

    if (log_level <= Log_Level::ERROR) {
        platform_console_write_error(out_message, level_colors[level_index]);
    } else {
        platform_console_write(out_message, level_colors[level_index]);
    }
}

}
