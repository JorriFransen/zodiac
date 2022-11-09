#pragma once

#include <defines.h>

namespace Zodiac
{

ZAPI void *platform_allocate(u64 size, u64 alignment = 1);
ZAPI void platform_free(void *memory);
ZAPI void *platform_memset(void *memory, i64 value, u64 size);
ZAPI void *platform_zero_mem(void *memory, u64 num);
ZAPI void *platform_memcpy(void *dest, const void *src, u64 num);
ZAPI i64 platform_memcmp(const void *a, const void *b, u64 num);

}