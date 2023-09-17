
#include "defines.h"

#include <cstdio>

#ifdef ZPLATFORM_LINUX
#define public extern "C" __attribute__((visibility("default")))
#elif ZPLATFORM_WINDOWS
#define public extern "C" __declspec(dllexport)
#endif

public s64 foreign_add(s64 a, s64 b) {
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

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!!!! This function is not being used at the moment !!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//  It will return true in some cases it should return false.
//  When stepping through the disassembled binary, it seems
//  like the register being used for the x argument has a value
//  of 256 (or maybe some other value where higher bits are set).
//  The bool type should be 1 byte so max 255.... (tried masking
//  here with 0xff but that didn't change anything.)
//     My best guess at the moment is that it has something to
//  do with struct packing/alignment, causing it to load more
//  data into the register than it should..
//
public const char *bool_to_string(bool x) {
    if (x) {
        return "true";
    }
    return "false";
}
