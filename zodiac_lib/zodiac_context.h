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

struct AST_File;
struct File_Handle;
struct Flat_Root_Node;
struct Resolver;
struct Type;
struct Type_Info;
struct Zodiac_Error;

namespace Bytecode {
    struct Bytecode_Builder;
    struct Bytecode_Converter;
}

using namespace Bytecode;

#define DEFAULT_OUTPUT_FILENAME "a.out" ZPLATFORM_DEFAULT_EXE_EXTENSION

struct Zodiac_Options
{
    String_Ref input_file_name = {};
    String_Ref output_file_name = DEFAULT_OUTPUT_FILENAME;
    bool output_filename_specified = false;

    bool dont_emit_binary = false;

    bool print_ast = false;
    bool print_bytecode = false;
    bool print_llvm_ir = false;
    bool verbose = false;

    // Temporary/Development options
    bool use_bool_to_string_for_PRINT_in_llvm = false;
    bool report_errors = true; // Used in the tests to not display errors for succeeding tests
    bool trace_bytecode_validation = false;
    bool validate_bytecode = true;
};

enum class File_To_Parse_Kind
{
    PATH,
    STRING,
};

struct File_To_Parse
{
    File_To_Parse_Kind kind;
    Atom path;
    String_Ref source;
};

struct Zodiac_Context
{
    Atom_Table atoms;

    Linear_Allocator ast_allocator_state;
    Allocator ast_allocator;

    Linear_Allocator bytecode_allocator_state;
    Allocator bytecode_allocator;

    Temporary_Allocator error_allocator_state;
    Allocator error_allocator;

    Zodiac_Options options = {};

    Dynamic_Array<File_To_Parse> files_to_parse;
    Dynamic_Array<AST_File *> parsed_files;

    Resolver *resolver;
    Bytecode_Builder *bytecode_builder;
    Bytecode_Converter *bytecode_converter;
    File_Handle *interp_stdout_file;

    Dynamic_Array<Zodiac_Error> errors;
    bool parse_error;
    bool fatal_resolve_error;

    String compiler_exe_path;
    String compiler_exe_dir;
    String module_dir;
    String builtin_module_path;

    String support_lib_dynamic_path;
#ifdef ZPLATFORM_WINDOWS
    String support_dll_dynamic_path;
#endif
    String support_lib_static_path;

    Type *builtin_string_type;
    Type *builtin_type_info_kind_type;
    Type *builtin_type_info_type;
    Type *builtin_type_info_int_type;
    Type *builtin_type_info_pointer_type;
    Type *builtin_type_info_struct_type;
    Type *builtin_type_info_struct_member_type;
    Type *builtin_type_info_enum_type;
    Type *builtin_type_info_enum_member_type;

    Dynamic_Array<Type_Info *> type_infos;
};

ZAPI void zodiac_context_create(Zodiac_Options options, Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

ZAPI bool zodiac_context_compile(Zodiac_Context *ctx, File_To_Parse ftp);
ZAPI bool zodiac_context_compile(Zodiac_Context *ctx, String_Ref code, Atom origin);
ZAPI bool zodiac_context_compile(Zodiac_Context *ctx, String_Ref code, const char *origin);
ZAPI bool zodiac_context_compile(Zodiac_Context *ctx);

ZAPI bool do_parse_jobs(Zodiac_Context *ctx);
ZAPI bool do_run_job(Zodiac_Context *ctx, Flat_Root_Node *root_node);

}
