#pragma once

#include "bytecode/bytecode.h"
#include "bytecode/ffi.h"
#include "containers/dynamic_array.h"
#include "containers/stack.h"
#include "defines.h"
#include "platform/filesystem.h"

namespace Zodiac {

struct Allocator;
struct Type;
struct Zodiac_Context;

namespace Bytecode {

typedef u64 Interpreter_Register_Flags;
enum Interpreter_Register_Flags_ : Interpreter_Register_Flags
{
    INTERP_REG_FLAG_NONE              = 0x00,
    INTERP_REG_FLAG_AGGREGATE_LITERAL = 0x01,
    INTERP_REG_FLAG_ZEROINITIALIZER   = 0x02,
};

struct Interpreter_Register
{
    Interpreter_Register_Flags flags = INTERP_REG_FLAG_NONE;
    Type *type = nullptr;

    union
    {
        Bytecode_Register_Value value = {};
        u8 *pointer;
    };
};

struct Interpreter_Stack_Frame
{
    s64 ip = 0;
    s64 bp = 0;
    Bytecode_Function_Handle fn_handle = -1;

    s64 register_start;
    s64 register_count;

    s64 stack_start;
    s64 stack_count;
    s64 sp;

    s64 dest_index = -1;
};

struct Interpreter
{
    Allocator *allocator = nullptr;
    Zodiac_Context *context = nullptr;

    bool running = false;

    Interpreter_Register start_return_value = {};
    Bytecode_Block_Handle jumped_from_block = -1;

    Array_Ref<Bytecode_Function> functions = {};
    Stack<Interpreter_Stack_Frame> frames = {};
    Stack<Interpreter_Register> arg_stack = {};

    Dynamic_Array<Interpreter_Register> globals = {};
    u8 *global_mem = nullptr;

    Array_Ref<Interpreter_Register> registers = {};
    s64 used_register_count = 0;

    Array_Ref<u8> stack_mem = {};
    s64 stack_mem_used = 0;

    File_Handle std_out;

    FFI_Context ffi = {};
};

ZAPI Interpreter interpreter_create(Allocator *allocator, Zodiac_Context *context);
ZAPI void interpreter_init(Allocator *allocator, Zodiac_Context *context, Interpreter *out_interp);
ZAPI void interpreter_free(Interpreter *interp);

ZAPI Interpreter_Register interpreter_start(Interpreter *interp, Bytecode_Program program);
ZAPI Interpreter_Register interpreter_start(Interpreter *interp, Bytecode_Program program, Bytecode_Function_Handle entry_handle);
ZAPI Interpreter_Register interpreter_start(Interpreter *interp, Array_Ref<Bytecode_Function> functions, Array_Ref<Bytecode_Function_Handle> foreign_functions, Array_Ref<Bytecode_Global> globals, s64 global_size, Bytecode_Function_Handle fn_handle);

ZAPI Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp);
ZAPI void interpreter_execute_instruction(Interpreter *interp, Bytecode_Instruction instruction);

ZAPI void interpreter_call_foreign_function(Interpreter *interp, Bytecode_Function_Handle fn_handle, s64 arg_count, s64 dest_index);
ZAPI void interpreter_call_pointer(Interpreter *interp, Bytecode_Register fn_ptr_reg, s64 arg_count, s64 dest_index);
ZAPI void interpreter_call_ffi(Interpreter *interp, FFI_Handle ffi_handle, s64 arg_count, s64 dest_index, Type *return_type);
ZAPI Interpreter_Register *interpreter_handle_ffi_callback(Interpreter *interp, Bytecode_Function_Handle fn_handle);

ZAPI Interpreter_Register interpreter_load_register(Interpreter *interp, Bytecode_Register bc_reg);
ZAPI void interpreter_load_pointer(Interpreter *interp, u8 *source, Interpreter_Register *dest, Type *type);
ZAPI void interpreter_store_register(Interpreter *interp, Interpreter_Register source, Bytecode_Register dest);
ZAPI void interpreter_store_pointer(Interpreter* interp, Interpreter_Register source, u8 *dest);

ZAPI void interpreter_push_stack_frame(Interpreter *interp, Bytecode_Function_Handle fn_handle,
                                  s64 arg_count, s64 result_index);
ZAPI Interpreter_Stack_Frame interpreter_pop_stack_frame(Interpreter *interp);

ZAPI void interpreter_copy_compound_literal_into_memory(Interpreter *interp, u8 *dest, Interpreter_Register source);

ZAPI void interpreter_print_from_memory(Interpreter *interp, u8* mem, Type *type, bool quote_strings = false);
ZAPI void interpreter_print_register(Interpreter *interp, Interpreter_Register reg, bool quote_strings = false);

}}
