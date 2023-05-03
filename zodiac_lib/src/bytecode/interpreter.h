#pragma once

#include "bytecode/bytecode.h"
#include "containers/stack.h"
#include "type.h"

namespace Zodiac { namespace Bytecode {

struct Interpreter_Register
{
    Type *type = nullptr;

    union
    {
        Bytecode_Register_Value value = {};
        uint8_t *pointer;
    };
};

struct Interpreter_Stack_Frame
{
    int64_t ip = 0;
    int64_t bp = 0;
    Bytecode_Function_Handle fn_handle = -1;

    Array_Ref<Interpreter_Register> registers = {};
    Array_Ref<uint8_t> stack_mem = {};
    uint8_t *sp = nullptr;

    int64_t dest_index = -1;
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
    uint8_t *global_mem = nullptr;

    Array_Ref<Interpreter_Register> registers = {};
    int64_t used_register_count = 0;

    Array_Ref<uint8_t> stack_mem = {};
    int64_t stack_mem_used = 0;

    File_Handle *std_out;

    // FFI_Context ffi = {};
};

Interpreter interpreter_create(Allocator *allocator, Zodiac_Context *context);
void interpreter_free(Interpreter *interp);

Interpreter_Register interpreter_start(Interpreter *interp, Bytecode_Program program);
Interpreter_Register interpreter_start(Interpreter *interp, Array_Ref<Bytecode_Function> functions, Array_Ref<Bytecode_Function_Handle> foreign_functions, Array_Ref<Bytecode_Global> globals, int64_t global_size, Bytecode_Function_Handle fn_handle);

Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp);
void interpreter_execute_instruction(Interpreter *interp, Bytecode_Instruction instruction);

void interpreter_call_foreign_function(Interpreter *interp, Bytecode_Function_Handle fn_handle, int64_t arg_count, int64_t dest_index);
void interpreter_call_pointer(Interpreter *interp, Bytecode_Register fn_ptr_reg, int64_t arg_count, int64_t dest_index);
// void interpreter_call_ffi(Interpreter *interp, FFI_Handle ffi_handle, int64_t arg_count, int64_t dest_index, Type *return_type);
Interpreter_Register *interpreter_handle_ffi_callback(Interpreter *interp, Bytecode_Function_Handle fn_handle);

Interpreter_Register interpreter_load_register(Interpreter *interp, Bytecode_Register bc_reg);
Interpreter_Register interpreter_load_pointer(Interpreter *interp, uint8_t *source, Type *type);
void interpreter_store_register(Interpreter *interp, Interpreter_Register source, Bytecode_Register dest);
void interpreter_store_pointer(Interpreter* interp, Interpreter_Register source, uint8_t *dest);

void interpreter_push_stack_frame(Interpreter *interp, Bytecode_Function_Handle fn_handle,
                                  int64_t arg_count, int64_t result_index);
Interpreter_Stack_Frame interpreter_pop_stack_frame(Interpreter *interp);

}}
