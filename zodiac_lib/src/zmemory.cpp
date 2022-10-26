#include "zmemory.h"

#include <cstdlib>

namespace Zodiac
{

void* kallocate(i64 size)
{
    return malloc(static_cast<size_t>(size));
}

void kfree(void* memory, i64 size)
{
    assert(size > 0);
    free(memory);
}

}
