
#include "defines.h"

#ifdef ZPLATFORM_LINUX
#define public extern "C"
#elif ZPLATFORM_WINDOWS
#define public extern "C" __declspec(dllexport)
#endif

public u64 foreign_add(u64 a, u64 b) {
    return a + b;
}
