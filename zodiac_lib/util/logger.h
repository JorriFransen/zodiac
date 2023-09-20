#pragma once

#include "defines.h"
#include "asserts.h"
#include "zstring.h"

namespace Zodiac
{

struct File_Handle;

enum class Log_Level
{
    FATAL = 0,
    ERROR = 1,
    WARN  = 2,
    INFO  = 3,
    DEBUG = 4,
    TRACE = 5,
};

ZAPI bool logging_system_initialize(Log_Level mll = Log_Level::INFO);
ZAPI void logging_system_set_max_level(Log_Level max_level);
ZAPI void logging_system_set_stdout_file(File_Handle out_file);
ZAPI void logging_system_set_stderr_file(File_Handle err_file);

ZAPI void log_message(Log_Level log_level, const char *file, s64 line, const String_Ref fmt, ...);

#define ZFATAL(fmt, ...) { \
    log_message(Log_Level::FATAL, __FILE__, __LINE__, fmt, ##__VA_ARGS__); \
    ZODIAC_ABORT(); \
}

#define ZERROR(fmt, ...) log_message(Log_Level::ERROR, __FILE__, __LINE__, fmt, ##__VA_ARGS__);
#define ZWARN(fmt, ...) log_message(Log_Level::WARN, __FILE__, __LINE__, fmt, ##__VA_ARGS__);
#define ZINFO(fmt, ...) log_message(Log_Level::INFO, __FILE__, __LINE__, fmt, ##__VA_ARGS__);
#define ZDEBUG(fmt, ...) log_message(Log_Level::DEBUG, __FILE__, __LINE__, fmt, ##__VA_ARGS__);
#define ZTRACE(fmt, ...) log_message(Log_Level::TRACE, __FILE__, __LINE__, fmt, ##__VA_ARGS__);

}
