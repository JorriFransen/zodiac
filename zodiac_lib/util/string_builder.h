#pragma once

#include <stdarg.h>

#include "defines.h"
#include "memory/allocator.h"
#include "zstring.h"

#define ZSTRINGBUILDER_DEFAULT_BLOCK_SIZE 4096
#define ZSTRINGBUILDER_DEFAULT_TEMP_SIZE 2048

namespace Zodiac
{

struct String_Builder_Block
{
    u8 *memory;
    s64 size;
    s64 used;

    String_Builder_Block *next_block;
};

struct String_Builder
{
    Allocator *allocator;
    Allocator temp_allocator;
    s64 new_block_size;

    s64 total_size;

    String_Builder_Block *first_block;
    String_Builder_Block *current_block;
};

ZAPI void string_builder_create(String_Builder *out_sb);
ZAPI void string_builder_create(String_Builder *out_sb, Allocator *allocator, s64 new_block_size = ZSTRINGBUILDER_DEFAULT_BLOCK_SIZE);
ZAPI void string_builder_destroy(String_Builder *sb);

ZAPI String string_builder_to_string(String_Builder *sb);
ZAPI String string_builder_to_string(Allocator *allocator, String_Builder *sb);

ZAPI void string_builder_append(String_Builder *sb, String_Ref fmt, ...);
ZAPI void string_builder_append(String_Builder *sb, String_Ref fmt, va_list args);
}
