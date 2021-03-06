#pragma once

#include "array.h"
#include "zodiac_string.h"

#include <cstddef>

namespace Zodiac
{

#ifdef _WIN32
#define EMPTY_STRING ( String{} )
#else
#define EMPTY_STRING ( (String){ nullptr, 0 } )
#endif

#define CMD_OPTION_LIST                                                                  \
    DEFINE_OPTION(bool, help, false, "Print this message")                               \
    DEFINE_OPTION(bool, verbose, false, "")                                              \
                                                                                         \
    DEFINE_OPTION(bool, run_bytecode, false,                                             \
                  "Execute the generated bytecode if a 'main' function is present")      \
                                                                                         \
    DEFINE_OPTION(bool, print_parse_tree, false, "Print the parse tree")                 \
    DEFINE_OPTION(bool, print_ast, false, "Print the parsed ast")                        \
    DEFINE_OPTION(bool, print_resolved_ast, false, "Print the resolved ast")             \
    DEFINE_OPTION(bool, print_scope, false, "Scope dump")                                \
    DEFINE_OPTION(bool, print_link_command, false, "Print the linker invocation")        \
    DEFINE_OPTION(bool, print_llvm, false, "Print the generated llvm ir")                \
    DEFINE_OPTION(bool, print_bytecode, false, "Print the generated bytecode")           \
    DEFINE_OPTION(bool, dont_emit_llvm, false, "Dont't emit llvm ir and binary")         \
    DEFINE_OPTION(bool, link_c, false, "Link with libc (creates a dynamic executable)")  \
    DEFINE_OPTION(String, exe_file_name, EMPTY_STRING, "Output executable name")         \


enum Option_Template_Kind
{
    OT_Kind_invalid,
    OT_Kind_bool,
    OT_Kind_String,
};

union Default_Option_Value
{
    bool _bool;
    String _String;
};

struct Option_Template
{
    Option_Template_Kind kind = OT_Kind_invalid;
    const char *name = nullptr;
    const char *description = nullptr;

    Default_Option_Value default_value;
    uint64_t option_offset = 0;
};

struct Options
{
    bool valid = true;
    String file_path = {};
    String zodiac_exe_path = {};

#define DEFINE_OPTION(type, name, default_value, desc) type name = (default_value);

    CMD_OPTION_LIST

#undef DEFINE_OPTION
};

static const Option_Template option_templates[] = {

#define OPTION_VALUE_bool(v) { ._bool = (v) }
#define OPTION_VALUE_String(v) { ._String = (v) }

#define DEFINE_OPTION(type, name, default_value, desc) \
    { OT_Kind_##type, #name, desc, OPTION_VALUE_##type(default_value), offsetof(Options, name) },

    CMD_OPTION_LIST

#undef DEFINE_OPTION
#undef OPTION_VALUE_bool
#undef OPTION_VALUE_String

};

Options parse_command_line(Allocator *allocator, int argc, char **argv);

#undef EMPTY_STRING
}

