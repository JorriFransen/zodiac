#include "platform.h"

#ifdef ZPLATFORM_WINDOWS

#include <common.h>

#include <cstring>
#include <stdlib.h>

#include <windows.h>
#include <io.h>

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
    platform_file_write(filesystem_stdout_file(), message);
}

void platform_console_write(const String_Ref message, Platform_Console_Color color)
{
    platform_file_write(filesystem_stdout_file(), message, color);
}

void platform_console_write_error(const String_Ref message)
{
    platform_file_write(filesystem_stderr_file(), message);
}

void platform_console_write_error(const String_Ref message, Platform_Console_Color color)
{
    platform_file_write(filesystem_stderr_file(), message, color);
}

}

#endif