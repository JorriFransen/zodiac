#pragma once

#include <defines.h>
#include <platform/filesystem.h>

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

ZAPI bool logging_system_initialize();
ZAPI void logging_system_set_stdout_file(File_Handle *out_file);
ZAPI void logging_system_set_stderr_file(File_Handle *err_file);

ZAPI void log_message(Log_Level level, const char *fmt, ...);

#define ZFATAL(fmt, ...) log_message(Log_Level::FATAL, fmt, ##__VA_ARGS__);
#define ZERROR(fmt, ...) log_message(Log_Level::ERROR, fmt, ##__VA_ARGS__);
#define ZWARN(fmt, ...) log_message(Log_Level::WARN, fmt, ##__VA_ARGS__);
#define ZINFO(fmt, ...) log_message(Log_Level::INFO, fmt, ##__VA_ARGS__);
#define ZDEBUG(fmt, ...) log_message(Log_Level::DEBUG, fmt, ##__VA_ARGS__);
#define ZTRACE(fmt, ...) log_message(Log_Level::DEBUG, fmt, ##__VA_ARGS__);

}
