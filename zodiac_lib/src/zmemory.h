#pragma once

#include <defines.h>

namespace Zodiac
{

ZAPI void* zallocate(i64 size);
ZAPI void zfree(void *memory, i64 size);
ZAPI void* zmemset(void *memory, i64 value, i64 num);
ZAPI void* zzeromem(void *memory, i64 num);
ZAPI void* zmemcpy(void *dest, const void *src, i64 num);
ZAPI i64 zmemcmp(const void *a, const void *b, i64);

}
