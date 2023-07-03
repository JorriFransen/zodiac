#include "platform.h"

#ifdef ZPLATFORM_LINUX

#include "common.h"
#include "defines.h"
#include "memory/temporary_allocator.h"
#include "platform/filesystem.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"


#include <cmath>
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

namespace Zodiac
{

struct Allocator;

OS_Release_Info os_release_info(Allocator *allocator)
{
    OS_Release_Info result = {};

    auto os_release_path = "/etc/os-release";

    if (!filesystem_exists(os_release_path)) {
        result.found = false;
        return result;
    }

    result.found = true;

    String os_release_content;
    bool read_res = filesystem_read_entire_file(temp_allocator_allocator(), os_release_path, &os_release_content);
    assert(read_res);

    int line_begin = 0;
    for (s64 i = 0; i < os_release_content.length; i++) {

        if (os_release_content[i] == '\n') {
            String_Ref line(&os_release_content[line_begin], i - line_begin);

#define MATCH_LINE_IF_(start, prop) \
    if (string_starts_with(line, (start))) { \
        String_Ref _start((start)); \
        result.prop = string_copy(allocator, &(line)[_start.length], (line).length - _start.length); \
    }

#define MATCH_LINE_IF_ELSE_(start, prop) \
    MATCH_LINE_IF_(start, prop) else

            MATCH_LINE_IF_ELSE_("NAME=", name)
            MATCH_LINE_IF_("ID=", id)

#undef MATCH_LINE_IF_
#undef MATCH_LINE_IF_ELSE_

            line_begin = i + 1;
        }

    }


    return result;
}

bool platform_info_generic(Allocator *allocator, Platform_Info *info)
{
    assert(allocator);
    assert(info);
    assert(info->allocator == nullptr);
    info->allocator = allocator;

    const char *candidate_paths[] = {
        "/usr/lib/",
        "/usr/lib64/",
        "/usr/lib/x86_64-linux-gnu/",
    };

    bool crt_found = false;

    const s64 candidate_count = sizeof(candidate_paths) / sizeof(candidate_paths[0]);
    for (s64 i = 0; i < candidate_count; i++) {
        auto path = String_Ref(candidate_paths[i]);

        if (!filesystem_is_link(path)) {
            auto scrt1_path = string_append(temp_allocator_allocator(), path, "Scrt1.o");

            if (filesystem_is_regular(scrt1_path)) {
                info->crt_path = string_copy(allocator, path);
                crt_found = true;
                break;
            }
        }
    }

    if (!crt_found) {
        info->err = "Failed to find crt path, required for linking with the c standard library.";
        return false;
    }

    auto dynamic_linker_path = "/lib64/ld-linux-x86-64.so.2";
    assert(filesystem_is_regular(dynamic_linker_path));
    info->dynamic_linker_path = string_copy(allocator, dynamic_linker_path);
    return true;
}

bool platform_info(Allocator *allocator, Platform_Info *info)
{
    assert(allocator);
    assert(info);
    assert(info->allocator == nullptr);

    if (getenv("ZODIAC_NIX_SHELL")) {
        assert_msg(false, "nixos_platform_info not implemented");
        // return nixos_find_crt_path(allocator, dest);
    }

    OS_Release_Info ori = os_release_info(temp_allocator_allocator());
    assert(ori.found);

    if (string_equal(ori.id, "nixos")) {
        assert_msg(false, "nixos_platform_info not implemented");
        // return nixos_find_crt_path(allocator, dest);
    }

    ZTRACE( "[platform_info()] Unsupported os '%s', falling back on platform_info_generic()\n", ori.id.data);
    return platform_info_generic(allocator, info);
}

struct Linear_Alloc_Header
{
    void *start; // NOTE: Since the size is stored in 32 bits, we should be able to store the lower 32 bits of the start addess, and replace those in the pointer passed to free.
    u32 size;
    u16 alignment;
};

void *platform_allocate(u64 size, u64 alignment/*=1*/)
{
    assert(size && alignment);
    assert(is_power_of_two(alignment));

    u64 total_size = size + alignment - 1 + sizeof(Linear_Alloc_Header);
    void *memory = malloc(total_size);
    assert(memory);
    u64 aligned_offset = get_aligned(((u64)memory) + sizeof(Linear_Alloc_Header), alignment);
    auto header = (Linear_Alloc_Header *)(aligned_offset - sizeof(Linear_Alloc_Header));
    header->start = memory;
    header->size = size;
    header->alignment = alignment;

    return (void *)aligned_offset;
}

void platform_free(void *memory)
{
    assert(memory);

    auto header = (Linear_Alloc_Header *)((u64)memory - sizeof(Linear_Alloc_Header));
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
    char name_template[] = "/tmp/ztmp-XXXXXX";
    int fd = mkstemp(name_template);
    if (fd == -1) {
        assert(false && !"mkstemp failed in create_temp_file!...");
    }

    FILE *result = fdopen(fd, "w+b");
    if (result == nullptr) {
        assert(false && !"fdopen failed in create_temp_file!...");
    }

    assert(unlink(name_template) == 0);

    *out_file = { result, true };
}

void platform_file_write(File_Handle *file, const String_Ref message)
{
    assert(file->valid && file->handle);
    assert(message.data);

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
    static const char *color_strings[6] = { "34", "32", "31", "31;1", "33", "38;5;245" };

    char str[ZSTRING_FORMAT_STACK_BUFFER_SIZE];
    u64 size = string_format(str, "\033[%sm%s\033[0m", color_strings[color_index], message);
    u64 size_written;

    filesystem_write(file, size, str, &size_written);
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
    char exe_path[PATH_MAX];
    ssize_t exe_path_length = readlink("/proc/self/exe", exe_path, PATH_MAX);
    assert(exe_path_length != -1);

    return String(allocator, exe_path, exe_path_length);
}

String platform_dir_name(Allocator *allocator, const String_Ref path)
{
    assert(filesystem_exists(path));
    assert(path.data[path.length] == '\0');

    // Dirname may copy the passed in string, so copy it first
    auto _path = string_copy(temp_allocator_allocator(), path);

    char *result = dirname((char *)_path.data);
    assert(result);

    return String(allocator, result);
}

void platform_exit(int exit_code)
{
    exit(exit_code);
}

}

#endif

