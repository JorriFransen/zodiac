#pragma once

#include <defines.h>

namespace Zodiac
{

ZAPI void* kallocate(u64 size);
ZAPI void kfree(void* memory, u64 size);

}
