#pragma once

#include <defines.h>

#include "util/zstring.h"

namespace Zodiac
{
struct Allocator;

struct File_Handle
{
    void *handle;
    bool valid;
};

enum File_Mode : u32
{
    FILE_MODE_READ  = 0x01,
    FILE_MODE_WRITE = 0x02,
};

ZAPI bool filesystem_exists(const String_Ref path);
ZAPI bool filesystem_is_link(const String_Ref path);
ZAPI bool filesystem_is_regular(const String_Ref path);
ZAPI bool filesystem_open(const String_Ref path, File_Mode mode, File_Handle *out_handle);
ZAPI bool filesystem_close(File_Handle *handle);
ZAPI bool filesystem_size(File_Handle *handle, u64 *out_size);
ZAPI void filesystem_flush(File_Handle *handle);

ZAPI void filesystem_remove(const String_Ref path);

ZAPI bool filesystem_read(File_Handle *handle, u64 size, u8 *out_bytes, u64 *out_size);
ZAPI bool filesystem_write(File_Handle *handle, u64 data_size, const void *data, u64 *out_size);

ZAPI bool filesystem_read_entire_file(Allocator *allocator, const String_Ref path, String *out_string);

ZAPI void filesystem_temp_file(File_Handle *out_file);

ZAPI void filesystem_stdout_file(File_Handle *out_file);
ZAPI void filesystem_stderr_file(File_Handle *out_file);

ZAPI String filesystem_exe_path(Allocator *allocator);
ZAPI String filesystem_dir_name(Allocator *allocator, const String_Ref path);

}
