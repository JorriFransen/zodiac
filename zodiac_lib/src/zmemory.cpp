#include "zmemory.h"

#include <cstdlib>

namespace Zodiac
{

void* kallocate(u64 size)
{
    return malloc(size);
}

void kfree(void* memory, u64 size)
{
    free(memory);
}

}
