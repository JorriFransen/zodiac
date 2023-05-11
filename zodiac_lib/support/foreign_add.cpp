
#include "defines.h"


#ifdef linux
#define public extern "C"
#elif _WIN32
#define public extern "C" __declspec(dllexport)
#endif

public u64 foreign_add(u64 a, u64 b) {
    return a + b;
}
