#include "filesystem.h"

#include "defines.h"
#include "memory/allocator.h"
#include "platform.h"
#include "util/asserts.h"
#include "util/logger.h"

#include <stdio.h>
#include <sys/stat.h>

#ifdef ZPLATFORM_WINDOWS

#define lstat stat

#ifndef S_ISREG
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif // S_ISREG

#ifdef S_IFLNK
#ifndef S_ISLNK
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif // S_ISLNK
#else
#define S_ISLNK(m) (0 && (m))
#endif // S_IFLNK

#endif // ZPLATFORM_WINDOWS

namespace Zodiac
{

bool filesystem_exists(const String_Ref path)
{
    assert(path.data[path.length] == '\0');

    struct stat statbuf;

    int stat_res = stat(path.data, &statbuf);

    return stat_res == 0;
}

ZAPI bool filesystem_is_link(const String_Ref path)
{
    assert(path.data[path.length] == '\0');

    struct stat statbuf;

    int stat_res = lstat(path.data, &statbuf);

    if (stat_res != 0) return false;

    auto mode = statbuf.st_mode;

    return S_ISLNK(mode);
}

ZAPI bool filesystem_is_regular(const String_Ref path)
{
    assert(path.data[path.length] == '\0');

    struct stat statbuf;

    int stat_res = lstat(path.data, &statbuf);

    if (stat_res != 0) return false;

    auto mode = statbuf.st_mode;

    return S_ISREG(mode);
}

bool filesystem_open(const String_Ref path, File_Mode mode, File_Handle *out_handle)
{
    assert(out_handle);

    out_handle->handle = nullptr;
    out_handle->valid = false;

    bool read = (mode & FILE_MODE_READ) != 0;
    bool write = (mode & FILE_MODE_WRITE) != 0;

    const char *mode_str;

    if (read && write) {
        mode_str = "w+b";
    } else if (read && !write) {
        mode_str = "rb";
    } else if (!read && write) {
        mode_str = "wb";
    } else {
        ZERROR("Invalid mode passed to filesystem_open(\"%s\")", path.data);
        return false;
    }

    FILE *file = fopen(path.data, mode_str);
    if (!file) {
        ZERROR("Error opening file: '%s'", path.data);
        return false;
    }

    out_handle->handle = file;
    out_handle->valid = true;

    return true;
}

bool filesystem_close(File_Handle *handle)
{
    assert(handle && handle->valid && handle->handle);

    auto res = fclose((FILE *)handle->handle);
    handle->handle = nullptr;
    handle->valid = false;

    return res == 0;
}

bool filesystem_size(File_Handle *handle, u64 *out_size)
{
    assert(handle && handle->valid && handle->handle);
    assert(out_size);

    if (fseek((FILE *)handle->handle, 0, SEEK_END) == -1) {
        ZERROR("fseek failed....");
        return false;
    }

    *out_size = ftell((FILE *)handle->handle);
    rewind((FILE *)handle->handle);

    return true;
}

void filesystem_flush(File_Handle *handle)
{
    assert(handle && handle->valid && handle->handle);

    fflush((FILE *)handle->handle);
}

void filesystem_remove(const String_Ref path)
{
    assert(filesystem_exists(path));
    remove(path.data);
}

bool filesystem_read(File_Handle *handle, u64 size, u8 *out_bytes, u64 *out_size)
{
    assert(handle && handle->valid && handle->handle);
    assert(out_bytes && out_size);

    if (!size) {
        *out_size = 0;
        return true;
    }

    *out_size = fread(out_bytes, 1, size, (FILE *)handle->handle);
    if (*out_size != size) {
        return false;
    }

    return true;
}

bool filesystem_write(File_Handle *handle, u64 data_size, const void *data, u64 *out_size)
{
    assert(handle && handle->valid && handle->handle);
    assert(data_size && data && out_size);

    *out_size = fwrite(data, 1, data_size, (FILE *)handle->handle);
    if (data_size != *out_size) {
        return false;
    }

    fflush((FILE *)handle->handle);
    return true;
}

bool filesystem_read_entire_file(Allocator *allocator, const String_Ref path, String *out_string)
{
    assert(allocator && path.data && out_string);
    assert(out_string->data == nullptr && out_string->length == 0);

    if (!filesystem_exists(path)) {
        ZERROR("Path does not exist: '%s', in read_entire_file()", path.data);
        return false;
    }

    File_Handle file_handle = {};
    bool open_result = filesystem_open(path, FILE_MODE_READ, &file_handle);
    assert(open_result);

    u64 size;
    bool size_result = filesystem_size(&file_handle, &size);
    assert(size_result);

    out_string->data = alloc_array<char>(allocator, size + 1);
    assert(out_string->data);

    u64 read_size;
    filesystem_read(&file_handle, size, (u8 *)out_string->data, &read_size);


    out_string->data[size] = '\0';
    out_string->length = size;

    return true;
}

void filesystem_temp_file(File_Handle *out_file)
{
    platform_temp_file(out_file);
}

void filesystem_stdout_file(File_Handle *out_file)
{
    assert(out_file && !out_file->valid);

    out_file->handle = stdout;
    out_file->valid = true;
}

void filesystem_stderr_file(File_Handle *out_file)
{
    assert(out_file && !out_file->valid);

    out_file->handle = stderr;
    out_file->valid = true;
}

String filesystem_exe_path(Allocator *allocator)
{
    return platform_exe_path(allocator);
}

String filesystem_dir_name(Allocator *allocator, const String_Ref path)
{
    return platform_dir_name(allocator, path);
}

String filesystem_cwd(Allocator *allocator)
{
    return platform_cwd(allocator);
}

}
