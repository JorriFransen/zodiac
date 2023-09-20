#include "zmemory.h"

#include "defines.h"
#include "memory/allocator.h"
#include "memory/dynamic_allocator.h"
#include "memory/temporary_allocator.h"

namespace Zodiac
{

bool memory_system_initialized = false;
Dynamic_Allocator dynamic_allocator_state;
Allocator dynamic_allocator;

bool memory_system_initialize()
{
    if (memory_system_initialized)  return true;

    bool result = dynamic_allocator_create(MEBIBYTE(1), &dynamic_allocator_state);
    assert(result);

    dynamic_allocator = dynamic_allocator_allocator(&dynamic_allocator_state);

    memory_system_initialized = true;

    // This should initialize the temporary allocator
    temp_allocator();

    return true;
}

void memory_system_deinitialize()
{
    deinitialize_global_temp_allocator();

    dynamic_allocator_destroy(&dynamic_allocator_state);

    memory_system_initialized = false;
}

}
