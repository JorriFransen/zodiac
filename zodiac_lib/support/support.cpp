
#include "defines.h"

#include <cstdio>

#ifdef ZPLATFORM_LINUX
#define public extern "C"
#elif ZPLATFORM_WINDOWS
#define public extern "C" __declspec(dllexport)
#endif

public u64 foreign_add(u64 a, u64 b) {
    return a + b;
}

static FILE *stdout_handle;
public void runtime_set_stdout(FILE *file) {
    stdout_handle = file;
}

typedef s64 (* Binop_FN_Ptr)(s64, s64);
public s64 foreign_call_binop_ptr(Binop_FN_Ptr fn_ptr, s64 a, s64 b) {
    fprintf(stdout_handle, "Calling pointer!\n");
    return fn_ptr(a, b);
}

public const char *bool_to_string(bool x) {
    return x ? "true" : "false";
}
