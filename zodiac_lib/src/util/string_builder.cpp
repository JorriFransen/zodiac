#include "string_builder.h"

#include "asserts.h"
#include "common.h"
#include "memory/linear_allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "util/zstring.h"

namespace Zodiac
{

file_local String_Builder_Block *sb_alloc_block(Allocator *allocator, s64 block_size);

void string_builder_create(String_Builder *out_sb)
{
    assert(out_sb);
    string_builder_create(out_sb, &dynamic_allocator);
}

void string_builder_create(String_Builder *out_sb, Allocator *allocator, s64 new_block_size /*= ZSTRINGBUILDER_DEFAULT_BLOCK_SIZE*/)
{
    assert(allocator);
    assert(out_sb);

    out_sb->allocator = allocator;
    auto temp_size = ZSTRINGBUILDER_DEFAULT_TEMP_SIZE;
    auto temp_allocator = alloc<Temporary_Allocator>(allocator);
    temporary_allocator_create(temp_size, alloc(allocator, temp_size), temp_allocator);
    out_sb->temp_allocator = temporary_allocator_allocator(temp_allocator);

    out_sb->new_block_size = new_block_size;
    out_sb->total_size = 0;
    out_sb->first_block = sb_alloc_block(allocator, out_sb->new_block_size);
    out_sb->current_block = out_sb->first_block;
}

void string_builder_destroy(String_Builder *sb)
{
    auto temp_allocator = (Temporary_Allocator *)sb->temp_allocator.user_data;
    free(sb->allocator, temp_allocator->linear_allocator.memory);

    temporary_allocator_destroy(temp_allocator);
    free(sb->allocator, temp_allocator);

    auto block = sb->first_block;
    while (block) {
        auto next = block->next_block;

        free(sb->allocator, block);

        block = next;
    }

    zzeromem(sb, sizeof(String_Builder));
}

String string_builder_to_string(String_Builder *sb)
{
    assert(sb && sb->allocator);
    return string_builder_to_string(sb->allocator, sb);
}

String string_builder_to_string(Allocator *allocator, String_Builder *sb)
{
    assert(allocator);
    assert(sb && sb->first_block && sb->total_size);

    String result;
    result.length = sb->total_size;
    result.data = alloc_array<char>(allocator, sb->total_size + 1);

    char *cur = result.data;

    auto block = sb->first_block;
    while (block) {
        auto next = block->next_block;
        zmemcpy(cur, block->memory, block->used);
        cur += block->used;
        block = next;
    }

    assert(cur == result.data + result.length);

    result.data[result.length] = '\0';

    return result;
}

void string_builder_append(String_Builder *sb, String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    string_builder_append(sb, fmt, args);

    va_end(args);
}

void string_builder_append(String_Builder *sb, String_Ref fmt, va_list args)
{
    assert_msg(fmt.data[fmt.length] == '\0', "Null terminated string expexted");

    auto ta = (Temporary_Allocator *)sb->temp_allocator.user_data;
    auto temp_mark = temporary_allocator_get_mark(ta);

    String temp_result = string_format(&sb->temp_allocator, fmt, args);

    if (temp_result.length > sb->current_block->size - sb->current_block->used) {

        // Copy the first part into the current block
        s64 size = sb->current_block->size - sb->current_block->used;
        zmemcpy(sb->current_block->memory + sb->current_block->used, temp_result.data, size);
        sb->current_block->used += size;
        sb->total_size += size;

        temp_result.data += size;
        temp_result.length -= size;
        assert(temp_result.length > 0);

        // Add a new block
        s64 new_block_size = max(sb->new_block_size, temp_result.length);
        if (new_block_size > sb->new_block_size) {
            sb->new_block_size = max(sb->new_block_size * 2, temp_result.length);
        }
        String_Builder_Block *new_block = sb_alloc_block(sb->allocator, new_block_size);
        sb->current_block->next_block = new_block;
        sb->current_block = new_block;

    }

    zmemcpy(sb->current_block->memory + sb->current_block->used, temp_result.data, temp_result.length);
    sb->current_block->used += temp_result.length;
    sb->total_size += temp_result.length;

    temporary_allocator_reset(ta, temp_mark);
}

file_local String_Builder_Block *sb_alloc_block(Allocator *allocator, s64 block_size)
{
    s64 total_size = block_size + sizeof(String_Builder_Block);
    void *memory = alloc(allocator, total_size);

    auto result = (String_Builder_Block *)memory;
    result->memory = (u8 *)memory + sizeof(String_Builder_Block);
    result->size = block_size;
    result->used = 0;
    result->next_block = nullptr;

    return result;
}

}
