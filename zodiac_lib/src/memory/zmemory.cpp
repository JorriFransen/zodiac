#include "zmemory.h"

namespace Zodiac
{

void memory_system_initialize()
{
    if (memory_system_initialized) assert(false && !"Memory system already initialized");

    bool result = dynamic_allocator_create(MEBIBYTE(1), &dynamic_allocator_state);
    assert(result);

    dynamic_allocator = dynamic_allocator_allocator(&dynamic_allocator_state);

    memory_system_initialized = true;
}

}
