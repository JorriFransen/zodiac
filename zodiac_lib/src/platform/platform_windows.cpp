#include "platform.h" // IWYU pragma: keep

#ifdef ZPLATFORM_WINDOWS

#include <windows.h>
#include <io.h>

#include "filesystem.h"

namespace Zodiac
{

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

i64 platform_memcmp(const void *a, const void *b, u64 num)
{
    assert(a && b);
    return memcmp(a, b, num);
}

double platform_sqrt(double x)
{
    return sqrt(x);
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
    TCHAR buf[buf_length];

    DWORD result = GetModuleFileNameW(nullptr, bu, buf_length);

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
    assert(false);
    return {};
}

void platform_exit(int exit_code)
{
    exit(exit_code);
}

}

#endif
