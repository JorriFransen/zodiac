#pragma once

#include "defines.h"

#ifdef ZPLATFORM_WINDOWS

#include "platform/microsoft_craziness.h"

namespace Zodiac
{

struct Allocator;

struct Platform_Info
{
    Windows_SDK_Info sdk_info;
    const char *err;
};

ZAPI String platform_windows_normalize_line_endings(Allocator *allocator, String_Ref str);

}

#endif // ZPLATFORM_WINDOWS