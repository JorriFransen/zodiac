#include "logger.h"

#include <stdarg.h>

#include "defines.h"
#include "platform/filesystem.h"
#include "platform/platform.h"
#include "util/asserts.h"
#include "util/zstring.h"

#define ZODIAC_LOGGER_FILE_LOCATIONS 1

namespace Zodiac
{

struct Logging_System_State
{
    File_Handle out_file;
    bool out_color;

    File_Handle err_file;
    bool err_color;
};

file_local bool logging_system_initialized = false;
file_local Logging_System_State logging_system_state;

file_local Log_Level max_log_level = Log_Level::INFO;

bool logging_system_initialize(Log_Level mll/*=Log_Level::INFO*/)
{
    if (logging_system_initialized) assert(false && !"Logging system already initialized");

    logging_system_set_max_level(mll);

    File_Handle out_file;
    File_Handle err_file;

    filesystem_stdout_file(&out_file);
    filesystem_stderr_file(&err_file);

    logging_system_set_stdout_file(out_file);
    logging_system_set_stderr_file(err_file);

    logging_system_initialized = true;

    return true;
}

void logging_system_set_max_level(Log_Level max_level)
{
    max_log_level = max_level;
}

void logging_system_set_stdout_file(File_Handle out_file)
{
    assert(out_file.valid && out_file.handle);

    logging_system_state.out_file = out_file;

    logging_system_state.out_color = isatty(fileno((FILE *)out_file.handle));
}

void logging_system_set_stderr_file(File_Handle err_file)
{
    assert(err_file.valid && err_file.handle);

    logging_system_state.err_file = err_file;

    logging_system_state.err_color = isatty(fileno((FILE *)err_file.handle));
}

void log_message(Log_Level log_level, const char *file, s64 line, const String_Ref fmt, ...)
{
    assert(logging_system_initialized);

    if (log_level > max_log_level) return;

    u64 level_index = (u64)log_level;
    assert(level_index >= 0 && log_level <= Log_Level::TRACE);

    const char *level_strings[6] = { "[FATAL]", "[ERROR]", "[WARN]", "[INFO]", "[DEBUG]", "[TRACE]"};

    const Platform_Console_Color level_colors[6] = {
        Platform_Console_Color::Fatal_Red,
        Platform_Console_Color::Red,
        Platform_Console_Color::Yellow,
        Platform_Console_Color::Green,
        Platform_Console_Color::Blue,
        Platform_Console_Color::Grey
    };

    char out_message[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);

    auto out_length = string_format(out_message, fmt, args);

    va_end(args);

    if (out_length + 1 > ZSTRING_FORMAT_STACK_BUFFER_SIZE) {
        assert(false && "Formatted log message does not fit in stack buffer");
        return;
    }

    String_Ref out_fmt = "%s: %s\n";
#define OUT_ARGS level_strings[level_index], out_message
#if ZODIAC_LOGGER_FILE_LOCATIONS
    out_fmt = "%s%s:%i: %s\n";
#undef OUT_ARGS
#define OUT_ARGS level_strings[level_index], file, line, out_message
#endif // ZODIAC_LOGGER_FILE_LOCATIONS

    out_length = string_format(out_message, out_fmt, OUT_ARGS);

#undef OUT_ARGS

    if (out_length + 1 > ZSTRING_FORMAT_STACK_BUFFER_SIZE) {
        assert(false && "Formatted log message does not fit in stack buffer");
        return;
    }

    if (log_level <= Log_Level::ERROR) {

        if (logging_system_state.err_color) platform_file_write(&logging_system_state.err_file, out_message, level_colors[level_index]);
        else platform_file_write(&logging_system_state.err_file, out_message);

    } else {
        if (logging_system_state.out_color) platform_file_write(&logging_system_state.out_file, out_message, level_colors[level_index]);
        else platform_file_write(&logging_system_state.out_file, out_message);
    }
}

void report_assert_fail(const char* expression, const char* message, const char *file, i64 line)
{
    log_message(Log_Level::FATAL, file, line, "Assertion failed: '%s', message: '%s'", expression, message);
    // fprintf(stderr, "[FATAL]%s:%lli: Assertion failed: '%s', message: '%s'\n", file, line, expression, message);
}

}
