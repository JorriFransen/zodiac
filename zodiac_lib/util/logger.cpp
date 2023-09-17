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
    File_Handle err_file;
    File_Handle log_file;
};

file_local bool logging_system_initialized = false;
file_local Logging_System_State logging_system_state;

file_local Log_Level max_log_level = Log_Level::INFO;

file_local void write_log_file(const String_Ref message);

bool logging_system_initialize(Log_Level mll/*=Log_Level::INFO*/)
{
    if (logging_system_initialized) assert(false && !"Logging system already initialized");

    logging_system_set_max_level(mll);

    filesystem_stdout_file(&logging_system_state.out_file);
    filesystem_stderr_file(&logging_system_state.err_file);

    if (!filesystem_open("console.log", FILE_MODE_WRITE, &logging_system_state.log_file)) {
        platform_console_write_error("ERROR: Unable to open 'console.log' for writing!", Platform_Console_Color::Red);
        return false;
    }

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
}

void logging_system_set_stderr_file(File_Handle err_file)
{
    assert(err_file.valid && err_file.handle);

    logging_system_state.err_file = err_file;
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
        platform_file_write(&logging_system_state.err_file, out_message, level_colors[level_index]);
    } else {
        platform_file_write(&logging_system_state.out_file, out_message, level_colors[level_index]);
    }

    write_log_file(out_message);
}

file_local void write_log_file(const String_Ref message)
{
    u64 out_size;
    bool result = filesystem_write(&logging_system_state.log_file, message.length, message.data, &out_size);
    assert(out_size == message.length);

    if (!result) {
        platform_console_write_error("ERROR: writing to 'console.log' failed!");
    }
}

void report_assert_fail(const char* expression, const char* message, const char *file, i64 line)
{
    log_message(Log_Level::FATAL, file, line, "Assertion failed: '%s', message: '%s'", expression, message);
}

}