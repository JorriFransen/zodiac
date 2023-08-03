#include "zodiac_context.h"

#include "lexer.h"
#include "platform/filesystem.h"
#include "type.h"
#include "util/asserts.h"
#include "util/zstring.h"

#include "error.h" // IWYU pragma: keep

namespace Zodiac
{

void zodiac_context_create(Zodiac_Context *out_context)
{
    assert(out_context);

    *out_context = {};

    atom_table_init(&out_context->atoms);

    if (!type_system_initialized) {
        bool result = type_system_initialize(out_context);
        assert(result);
    }

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->ast_allocator_state);
    out_context->ast_allocator = linear_allocator_allocator(&out_context->ast_allocator_state);

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->bytecode_allocator_state);
    out_context->bytecode_allocator = linear_allocator_allocator(&out_context->bytecode_allocator_state);

    temporary_allocator_create(KIBIBYTE(64), nullptr, &out_context->temp_allocator_state);
    out_context->temp_allocator = temporary_allocator_allocator(&out_context->temp_allocator_state);

    temporary_allocator_create(KIBIBYTE(8), nullptr, &out_context->error_allocator_state);
    out_context->error_allocator = temporary_allocator_allocator(&out_context->error_allocator_state);

    dynamic_array_create(c_allocator(), &out_context->errors);
    out_context->fatal_resolve_error = false;

    zodiac_register_keywords(&out_context->atoms);

    out_context->compiler_exe_path = filesystem_exe_path(c_allocator());
    assert(filesystem_exists(out_context->compiler_exe_path));

    out_context->compiler_exe_dir = filesystem_dir_name(c_allocator(), out_context->compiler_exe_path);
    assert(filesystem_exists(out_context->compiler_exe_dir));

#ifdef ZPLATFORM_LINUX
    auto dynamic_support_lib_name = "/libzrs.so";
    auto static_support_lib_name = "/libzrs_s.a";

#elif ZPLATFORM_WINDOWS
    auto dynamic_support_lib_name = "\\libzrs.lib";
    auto dynamic_support_dll_name = "\\libzrs.dll";
    auto static_support_lib_name = "\\libzrs_s.lib";

    out_context->support_dll_dynamic_path = string_append(c_allocator(), out_context->compiler_exe_dir, dynamic_support_dll_name);
    assert(filesystem_exists(out_context->support_dll_dynamic_path));

#endif

    out_context->support_lib_dynamic_path = string_append(c_allocator(), out_context->compiler_exe_dir, dynamic_support_lib_name);
    assert(filesystem_exists(out_context->support_lib_dynamic_path));

    out_context->support_lib_static_path = string_append(c_allocator(), out_context->compiler_exe_dir, static_support_lib_name);
    assert(filesystem_exists(out_context->support_lib_static_path));
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    // TODO: This should not be global state, but part of the context...
    type_system_initialized = false;

    atom_table_free(&context->atoms);

    auto ca = c_allocator();

    if (context->options.input_file_name.length) free(ca, context->options.input_file_name.data);
    if (context->options.output_file_name.length) free(ca, context->options.output_file_name.data);

    dynamic_array_free(&context->errors);

    free(ca, context->compiler_exe_path.data);
    free(ca, context->compiler_exe_dir.data);

    linear_allocator_destroy(&context->ast_allocator_state);
    linear_allocator_destroy(&context->bytecode_allocator_state);
    linear_allocator_destroy(&context->temp_allocator_state.linear_allocator);
}

}
