#pragma once

#include "defines.h"
#include "util/zstring.h"

#if ZPLATFORM_LINUX
#include "platform/platform_linux.h" // IWYU pragma: export
#elif ZPLATFORM_WINDOWS
#include "platform/platform_windows.h" // IWYU pragma: export
#endif

namespace Zodiac
{

struct File_Handle;
struct Platform_Info;

template<typename T> struct Array_Ref;

enum class Platform_Console_Color
{
    Blue,
    Green,
    Red,
    Fatal_Red,
    Yellow,
    Grey,
};

struct Process_Result
{
    int64_t exit_code = 0;
    bool success = false;

    String result_string = {};
    String error_string = {};
};

ZAPI bool platform_info(Allocator *allocator, Platform_Info *info);
ZAPI void free_platform_info(Platform_Info *info);

ZAPI void *platform_allocate(u64 size, u64 alignment = 1);
ZAPI void platform_free(void *memory);
ZAPI void *platform_memset(void *memory, i64 value, u64 size);
ZAPI void *platform_zero_mem(void *memory, u64 num);
ZAPI void *platform_memcpy(void *dest, const void *src, u64 num);
ZAPI void *platform_memmove(void *dest, const void *src, u64 num);
ZAPI i64 platform_memcmp(const void *a, const void *b, u64 num);

ZAPI double platform_sqrt(double x);

ZAPI Process_Result platform_execute_process(Array_Ref<String_Ref> *command_line);
ZAPI void platform_free_process_result(Process_Result *pr);

ZAPI void platform_temp_file(File_Handle *out_file);

ZAPI void platform_file_write(File_Handle *file, const String_Ref message);
ZAPI void platform_file_write(File_Handle *file, const String_Ref message, Platform_Console_Color color);

ZAPI void platform_console_write(const String_Ref message);
ZAPI void platform_console_write(const String_Ref message, Platform_Console_Color color);

ZAPI void platform_console_write_error(const String_Ref message);
ZAPI void platform_console_write_error(const String_Ref message, Platform_Console_Color color);

ZAPI String platform_exe_path(Allocator *allocator);
ZAPI String platform_dir_name(Allocator *allocator, const String_Ref path);

ZAPI void platform_exit(int exit_code);

}
