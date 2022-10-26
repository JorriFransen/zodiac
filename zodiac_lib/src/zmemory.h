#pragma once

#include <defines.h>

namespace Zodiac
{

ZAPI void* kallocate(i64 size);
ZAPI void kfree(void* memory, i64 size);

}
