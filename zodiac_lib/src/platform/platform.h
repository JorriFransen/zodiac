#pragma once

#include "defines.h"
#include "util/zstring.h"

namespace Zodiac
{

struct File_Handle;

ZAPI void *platform_allocate(u64 size, u64 alignment = 1);
ZAPI void platform_free(void *memory);
ZAPI void *platform_memset(void *memory, i64 value, u64 size);
ZAPI void *platform_zero_mem(void *memory, u64 num);
ZAPI void *platform_memcpy(void *dest, const void *src, u64 num);
ZAPI i64 platform_memcmp(const void *a, const void *b, u64 num);

ZAPI double platform_sqrt(double x);

enum class Platform_Console_Color
{
    Blue,
    Green,
    Red,
    Fatal_Red,
    Yellow,
    Grey,
};

ZAPI void platform_temp_file(File_Handle *out_file);

ZAPI void platform_file_write(File_Handle *file, const String_Ref message);
ZAPI void platform_file_write(File_Handle *file, const String_Ref message, Platform_Console_Color color);

ZAPI void platform_console_write(const String_Ref message);
ZAPI void platform_console_write(const String_Ref message, Platform_Console_Color color);

ZAPI void platform_console_write_error(const String_Ref message);
ZAPI void platform_console_write_error(const String_Ref message, Platform_Console_Color color);

ZAPI void platform_exit(int exit_code);

}
