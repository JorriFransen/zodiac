#include "platform.h" // IWYU pragma: keep

#ifdef ZPLATFORM_WINDOWS

#include <windows.h>

#include <io.h>
#include <libloaderapi.h>

#include "containers/dynamic_array.h"
#include "filesystem.h"
#include "memory/temporary_allocator.h"
#include "util/zstring.h"

#define MICROSOFT_CRAZINESS_IMPLEMENTATION
#pragma warning(push)
#pragma warning(disable:4189)
#pragma warning(disable:4456)
#include "platform/microsoft_craziness.h"
#undef VOID
#pragma warning(pop)

namespace Zodiac
{

bool platform_info(Allocator *allocator, Platform_Info *info)
{
    assert(allocator);
    assert(info);

    auto sdk = find_visual_studio_and_windows_sdk();
    assert(sdk.windows_sdk_root);
    info->sdk_info = sdk;

    return true;
}

void free_platform_info(Platform_Info *info)
{
    assert(info);

    free_resources(&info->sdk_info);
}

struct Alloc_Header
{
    void *start; // NOTE: Since the size is stored in 32 bits, we should be able to store the lower 32 bits of the start addess, and replace those in the pointer passed to free.
    u32 size;
    u16 alignment;
};

void *platform_allocate(u64 size, u64 alignment/*=1*/)
{
    assert(size && alignment);
    assert(is_power_of_two(alignment));

    u64 total_size = size + alignment - 1 + sizeof(Alloc_Header);
    void *memory = malloc(total_size);
    assert(memory);
    u64 aligned_offset = get_aligned(((u64)memory) + sizeof(Alloc_Header), alignment);
    auto header = (Alloc_Header *)(aligned_offset - sizeof(Alloc_Header));
    header->start = memory;
    header->size = size;
    header->alignment = alignment;

    return (void *)aligned_offset;
}

void platform_free(void *memory)
{
    assert(memory);

    auto header = (Alloc_Header *)((u64)memory - sizeof(Alloc_Header));
    ::free(header->start);
}

void *platform_memset(void *memory, i64 value, u64 size)
{
    assert(memory && size);
    return memset(memory, value, size);
}

void *platform_zero_mem(void *memory, u64 num)
{
    assert(memory);
    return platform_memset(memory, 0, num);
}

void *platform_memcpy(void *dest, const void *src, u64 num)
{
    assert(dest && src);
    return memcpy(dest, src, num);
}

void *platform_memmove(void *dest, const void *src, u64 num)
{
    assert(dest && src);
    return memmove(dest, src, num);
}

i64 platform_memcmp(const void *a, const void *b, u64 num)
{
    assert(a && b);
    return memcmp(a, b, num);
}

double platform_sqrt(double x)
{
    return sqrt(x);
}

Process_Result platform_execute_process(Array_Ref<String_Ref> *command_line_)
{
    auto command_line = *command_line_;
    assert(command_line.count);
    
    Process_Result result = {};

    PROCESS_INFORMATION process_info;
    STARTUPINFOW startup_info;
    ZeroMemory(&startup_info, sizeof(startup_info));
    ZeroMemory(&process_info, sizeof(process_info));
    startup_info.cb = sizeof(startup_info);

    auto ta = temp_allocator_allocator();

    Wide_String cmd_str(ta, command_line[0]);
    auto args = *command_line_; 
    args.count -= 1;
    args.data = &args.data[1];
    Wide_String arg_str(ta, string_append(ta, args, " "));

    bool proc_res = CreateProcessW(cmd_str.data, (LPWSTR)arg_str.data,
        nullptr, nullptr, true, 0, nullptr,
        nullptr, &startup_info, &process_info);

    if (!proc_res) {
        auto err = GetLastError();
        LPSTR message_buf = nullptr;
        size_t size = FormatMessageA((FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_IGNORE_INSERTS),
            nullptr, err, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPSTR)&message_buf, 0, nullptr);

        fprintf(stderr, "%.*s", (int)size, message_buf);
        LocalFree(message_buf);

        result.success = false;

    }
    else {
        WaitForSingleObject(process_info.hProcess, INFINITE);

        DWORD exit_code;
        GetExitCodeProcess(process_info.hProcess, &exit_code);

        CloseHandle(process_info.hProcess);
        CloseHandle(process_info.hThread);

        result.exit_code = exit_code;
        if (exit_code == 0) {
            result.success = true;
        }
        else {
            result.success = false;
        }
    }


    return result;
}

void platform_temp_file(File_Handle *out_file)
{
    const auto temp_path_length_ = 2048;
    TCHAR temp_path[temp_path_length_];

    DWORD temp_path_length = GetTempPath(temp_path_length_, temp_path);
    if (temp_path_length > MAX_PATH || temp_path_length <= 0) {
        assert(false && !"GetTempPath failed in create_temp_file!...");
    }

    TCHAR file_path[MAX_PATH];
    if (GetTempFileName(temp_path, TEXT("ZT"), 0, file_path) == 0) {
        assert(false && !"GetTempFileName failed in create_temp_file!...");
    }

    HANDLE file_handle = CreateFile(file_path, GENERIC_READ | GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS,
                                    FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE, nullptr);
    if (file_handle == INVALID_HANDLE_VALUE) {
        // GetLastError();
        assert(false && !"CreateFile failed in create_temp_file!...");
    }

    int file_descriptor = _open_osfhandle((intptr_t)file_handle, 0);
    if (file_descriptor == -1) {
        assert(false && !"_open_osfhandle failed in create_temp_file!...");
    }

    FILE* result = _fdopen(file_descriptor, "w+b");
    if (!result) {
        assert(false && !"_fdopen failed in create_temp_file!...");
    }

    *out_file = { .handle = result, .valid = true };
}

void platform_file_write(File_Handle *file, const String_Ref message)
{
    assert(file->valid && file->handle);

    u64 size_written;
    filesystem_write(file, message.length, message.data, &size_written);
    assert(size_written == message.length);
}

void platform_file_write(File_Handle *file, const String_Ref message, Platform_Console_Color color)
{
    assert(file->valid && file->handle);
    assert(message.data);

    u64 color_index = (u64)color;
    assert(color_index >= 0 && color_index < 6);
    static const char *color_strings[6] = { "34", "32", "31", "31;1", "33", "30;1" };

    char str[ZSTRING_FORMAT_STACK_BUFFER_SIZE];
    u64 size = string_format(str, "\033[%sm%s\033[0m", color_strings[color_index], message.data);
    u64 size_written;

    filesystem_write(file, size, str, &size_written);
    OutputDebugStringA(message.data);
    assert(size_written == size);
}

void platform_console_write(const String_Ref message)
{
    assert(message.data);

    File_Handle zstdout;
    filesystem_stdout_file(&zstdout);
    platform_file_write(&zstdout, message);
}

void platform_console_write(const String_Ref message, Platform_Console_Color color)
{
    assert(message.data);

    File_Handle zstdout;
    filesystem_stdout_file(&zstdout);
    platform_file_write(&zstdout, message, color);
}

void platform_console_write_error(const String_Ref message)
{
    assert(message.data);

    File_Handle zstderr;
    filesystem_stderr_file(&zstderr);
    platform_file_write(&zstderr, message);
}

void platform_console_write_error(const String_Ref message, Platform_Console_Color color)
{
    assert(message.data);

    File_Handle zstderr;
    filesystem_stderr_file(&zstderr);
    platform_file_write(&zstderr, message, color);
}

String platform_exe_path(Allocator *allocator)
{
    const auto buf_length = 2048;
    WCHAR buf[buf_length];

    DWORD result = GetModuleFileNameW(nullptr, buf, buf_length);

    if (result >= 0 && result < buf_length) {
        return String(allocator, buf, result);
    } else {
        auto err = GetLastError();
        if (err == ERROR_INSUFFICIENT_BUFFER) {
            assert_msg(false, "this_exe_path() failed with error: ERROR_INSUFFICIENT_BUFFER");
        } else {
            assert_msg(false, "this_exe_path() failed with unknown error");
        }
        exit(1);
        return {};
    }
}

String platform_dir_name(Allocator *allocator, const String_Ref path)
{
    Wide_String wide_path = Wide_String(temp_allocator_allocator(), path);

    WCHAR drive_name[_MAX_DRIVE];
    WCHAR dir_name[_MAX_DIR];

    _wsplitpath_s(wide_path.data, drive_name, _MAX_DRIVE, dir_name, _MAX_DIR, nullptr, 0, nullptr, 0);

    Wide_String result = string_append(temp_allocator_allocator(), Wide_String_Ref(drive_name), Wide_String_Ref(dir_name));

    return String(allocator, result.data, result.length);
}

void platform_exit(int exit_code)
{
    exit(exit_code);
}

}

#endif
