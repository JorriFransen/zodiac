#pragma once

#include "atom.h"
#include "defines.h"
#include "containers/dynamic_array.h"
#include "memory/allocator.h"
#include "memory/linear_allocator.h"
#include "memory/temporary_allocator.h"
#include "util/zstring.h"

namespace Zodiac
{

struct Zodiac_Error;

struct Zodiac_Context
{
    Atom_Table atoms;

    Linear_Allocator ast_allocator_state;
    Allocator ast_allocator;

    Temporary_Allocator temp_allocator_state;
    Allocator temp_allocator;

    Temporary_Allocator error_allocator_state;
    Allocator error_allocator;

    Dynamic_Array<Zodiac_Error> errors;
    bool fatal_resolve_error;

    String compiler_exe_path;
    String compiler_exe_dir;
    String support_lib_dynamic_path;
};

ZAPI void zodiac_context_create(Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

}
