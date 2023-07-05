#pragma once

#include "defines.h"

#ifdef ZPLATFORM_WINDOWS

#include "platform/microsoft_craziness.h"

namespace Zodiac
{

struct Platform_Info
{
    Windows_SDK_Info sdk_info;
    const char *err;
};

}

#endif // ZPLATFORM_WINDOWS