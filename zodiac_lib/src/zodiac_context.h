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

static String_Ref default_ouput_file_name = "a.out" ZPLATFORM_DEFAULT_EXE_EXTENSION;

struct Zodiac_Options
{
    String_Ref input_file_name = {};
    String_Ref output_file_name = string_copy(c_allocator(), default_ouput_file_name);

    bool dont_emit_binary = false;

    bool print_ast = false;
    bool print_bytecode = false;
    bool print_llvm_ir = false;
    bool verbose = false;

    // Temporary/Development options
    bool use_bool_to_string_for_PRINT_in_llvm = false;
};

struct Resolver;
struct Zodiac_Error;

struct Zodiac_Context
{
    Atom_Table atoms;

    Linear_Allocator ast_allocator_state;
    Allocator ast_allocator;

    Linear_Allocator bytecode_allocator_state;
    Allocator bytecode_allocator;

    Temporary_Allocator temp_allocator_state;
    Allocator temp_allocator;

    Temporary_Allocator error_allocator_state;
    Allocator error_allocator;

    Zodiac_Options options = {};
    Resolver *resolver;

    Dynamic_Array<Zodiac_Error> errors;
    bool fatal_resolve_error;

    String compiler_exe_path;
    String compiler_exe_dir;

    String support_lib_dynamic_path;
#ifdef ZPLATFORM_WINDOWS
    String support_dll_dynamic_path;
#endif
    String support_lib_static_path;
};

ZAPI void zodiac_context_create(Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

}
