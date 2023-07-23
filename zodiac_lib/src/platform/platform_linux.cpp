#include "platform.h"

#ifdef ZPLATFORM_LINUX

#include "common.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "platform/filesystem.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/string_builder.h"
#include "util/zstring.h"

#include <cmath>
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

namespace Zodiac
{

OS_Release_Info os_release_info(Allocator *allocator)
{
    assert(allocator);

    OS_Release_Info result = {};
    result.allocator = allocator;

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

    bool dynamic_linker_found = false;

    for (s64 i = 0; i < candidate_count; i++) {
        auto path = String_Ref(candidate_paths[i]);

        if (!filesystem_is_link(path)) {
            auto dynamic_linker_path = string_append(temp_allocator_allocator(), path, "ld-linux-x86-64.so.2");

            if (filesystem_exists(dynamic_linker_path)) {
                info->dynamic_linker_path = string_copy(allocator, dynamic_linker_path);
                dynamic_linker_found = true;
                break;
            }
        }
    }

    if (!dynamic_linker_found) {
        info->err = "Failed to find dynamic linker path, required for linking with c standard library.";
        return false;
    }

    return true;
}

struct Sup_Plat_Info {
    const char *name;
    const u64 hash;
};

#define SUPPORTED_PLATFORM_LIST \
    SUP_PLAT("arch") \
    SUP_PLAT("ubuntu") \
    SUP_PLAT("debian") \
    SUP_PLAT("fedora")

#define SUP_PLAT(p) { (p), hash_string(p) },

file_local Sup_Plat_Info supported_platforms[] = {
    SUPPORTED_PLATFORM_LIST
};

#undef SUP_PLAT

#define SUPPORTED_PLATFORM_COUNT (sizeof(supported_platforms) / sizeof(supported_platforms[0]))


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

    u64 platform_hash = hash_string(ori.id.data, ori.id.length);

    for (s64 i = 0; i < SUPPORTED_PLATFORM_COUNT; i++) {
        if (platform_hash == supported_platforms[i].hash) {

            // Just in case there is a hash collision
            assert(string_equal(ori.id, supported_platforms[i].name));

            ZTRACE( "[platform_info()] using platform_info_generic for os '%s'", supported_platforms[i].name);
            return platform_info_generic(allocator, info);
        }
    }

    if (string_equal(ori.id, "nixos")) {
        assert_msg(false, "nixos_platform_info not implemented");
        // return nixos_find_crt_path(allocator, dest);
    }

    ZTRACE( "[platform_info()] Unsupported os '%s', falling back on platform_info_generic()", ori.id.data);
    return platform_info_generic(allocator, info);
}

void free_platform_info(Platform_Info *info)
{
    assert(info);
    assert(info->allocator);

    free(info->allocator, info->crt_path.data);
    free(info->allocator, info->dynamic_linker_path.data);
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
    assert(command_line_);

    auto ta = temp_allocator_allocator();
    auto ca = c_allocator();

    auto command_line = *command_line_;
    assert(command_line.count);
    pid_t pid;

#define NUM_PIPES 3
#define READ_FD 0
#define WRITE_FD 1
#define STDIN_PIPE 0
#define STDOUT_PIPE 1
#define STDERR_PIPE 2

    int pipes[NUM_PIPES][2];

#define PARENT_STDOUT_FD ( pipes[STDOUT_PIPE][READ_FD] )
#define PARENT_STDIN_FD ( pipes[STDIN_PIPE][WRITE_FD] )
#define PARENT_STDERR_FD ( pipes[STDERR_PIPE][READ_FD] )

#define CHILD_STDOUT_FD ( pipes[STDOUT_PIPE][WRITE_FD] )
#define CHILD_STDIN_FD ( pipes[STDIN_PIPE][READ_FD] )
#define CHILD_STDERR_FD ( pipes[STDERR_PIPE][WRITE_FD] )

    pipe(pipes[STDIN_PIPE]);
    pipe(pipes[STDOUT_PIPE]);
    pipe(pipes[STDERR_PIPE]);

    char **argv;

    if ((pid = fork()) == -1) {
        ZFATAL("Fork error");
    } else if (pid == 0) {
        // New child process

        dup2(CHILD_STDIN_FD, STDIN_FILENO);
        dup2(CHILD_STDOUT_FD, STDOUT_FILENO);
        dup2(CHILD_STDERR_FD, STDERR_FILENO);

        close(CHILD_STDIN_FD);
        close(CHILD_STDOUT_FD);
        close(CHILD_STDERR_FD);
        close(PARENT_STDOUT_FD);
        close(PARENT_STDIN_FD);
        close(PARENT_STDERR_FD);

        argv = alloc_array<char*>(c_allocator(), command_line.count + 1);

        for (s64 i = 0; i < command_line.count; i++) {
            argv[i] = (char *)command_line[i].data;
        }

        argv[command_line.count] = 0;

        execvp(argv[0], (char *const *)argv);
        auto cmd_line_str = string_append(c_allocator(), command_line);
        ZERROR("execv error: '%s'", cmd_line_str.data); // We don't expect execv to return
        exit(1);

    } else {
        // Original parent process
        // ZINFO("Parent!");

        close(CHILD_STDIN_FD);
        close(CHILD_STDOUT_FD);
        close(CHILD_STDERR_FD);

        // We don't write to stdin of the child
        close(PARENT_STDIN_FD);

        String_Builder stdout_sb;
        string_builder_create(&stdout_sb, ta);

        String_Builder stderr_sb;
        string_builder_create(&stderr_sb, ta);

        int exit_status;

        // Wait for the child process to finish
        pid_t wait_res = waitpid(pid, &exit_status, 0);
        if (wait_res == -1) {
            ZFATAL("Unexpected return value from waitpid");
        } 

        { // read stdout
            size_t read_count;
            char buffer[1024];
            do {
                read_count = read(PARENT_STDOUT_FD, buffer, sizeof(buffer) - 1);
                if (read_count) {
                    string_builder_append(&stdout_sb, "%.*s", read_count, buffer);
                }
            } while (read_count > 0);
        }

        { // read stderr
            size_t read_count;
            char buffer[1024];
            do {
                read_count = read(PARENT_STDERR_FD, buffer, sizeof(buffer) - 1);
                if (read_count) {
                    string_builder_append(&stderr_sb, "%.*s", read_count, buffer);
                }
            } while (read_count > 0);
        }

        Process_Result result;
        result.exit_code = WEXITSTATUS(exit_status);
        result.success = result.exit_code == 0;

        if (stdout_sb.total_size) result.result_string = string_builder_to_string(ca, &stdout_sb);
        if (stderr_sb.total_size) result.error_string = string_builder_to_string(ca, &stderr_sb);

        return result;
    }

    assert(false);
    return {};

#undef NUM_PIPES
#undef READ_FD
#undef WRITE_FD
#undef STDIN_PIPE
#undef STDOUT_PIPE
#undef STDERR_PIPE
#undef PARENT_STDOUT_FD
#undef PARENT_STDIN_FD
#undef CHILD_STDOUT_FD
#undef CHILD_STDIN_FD
}

void platform_free_process_result(Process_Result *pr)
{
    assert(pr);

    if (pr->result_string.data) free(c_allocator(), pr->result_string.data);
    if (pr->error_string.data) free(c_allocator(), pr->error_string.data);

    *pr = {};
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

