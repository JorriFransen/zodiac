#pragma once

#include "util/zstring.h"

namespace Zodiac
{

struct Allocator;

struct Platform_Info
{
    Allocator *allocator;
    String crt_path;
    String dynamic_linker_path;

    const char *err;
};

struct OS_Release_Info
{
    Allocator *allocator;
    bool found = false;
    String name = {};
    String id = {};
};

}
