#pragma once

#include "allocator.h"
#include "bytecode.h"
#include "build_data.h"
#include "ffi.h"

namespace Zodiac
{
    struct Interp_Instruction_Pointer
    {
        int64_t index = 0;
        BC_Block *block = nullptr;
    };

    struct Interpreter_Value
    {
        AST_Type *type = nullptr;

        union
        {
            Integer_Literal integer_literal = {};
            Float_Literal float_literal;
            const char *string_literal;
            void *pointer;
            bool boolean_literal;
        };
    };

    enum class Interp_LValue_Kind
    {
        INVALID,
        TEMP,
        ALLOCL,
        PARAM,
        GLOBAL,
    };

    struct Interpreter_LValue
    {
        Interp_LValue_Kind kind = Interp_LValue_Kind::INVALID;
        AST_Type *type = nullptr;
        int64_t index = 0;
    };

    struct Interp_Stack_Frame
    {
        BC_Function *function = nullptr;
        Interp_Instruction_Pointer ip = {};
        int64_t first_temp_index = 0;
        int64_t result_index = -1;
        uint8_t *previous_alloc_sp = nullptr;

        Interpreter_Value *args = nullptr;
        int64_t arg_count = 0;

        Interpreter_Value *locals = nullptr;
        int64_t local_count = 0;
    };

    struct Interpreter
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        bool running = false;
        bool aborted = false;
        bool forked = false;
        bool exited = false;
        bool globals_initialized = false;
        bool foreigns_initialized = false;

        Stack<Interpreter_Value> temp_stack = {};
        Stack<Interpreter_Value> arg_stack = {};
        Stack<Interp_Stack_Frame> frames = {};

        Array<Interpreter_Value> globals = {};
        uint8_t *global_mem = nullptr;

        // @TODO: @CLEANUP: This should really be a stack allocator, we can
        //                   'save' restore points in the stack frames.
        //                   This way we can keep growing, without invalidating
         //                  previously allocated stack space.
        uint8_t *alloc_stack = nullptr;
        uint8_t *alloc_sp = nullptr;
        uint8_t *alloc_stack_end = nullptr;

        FFI_Context ffi = {};

        Hash_Table<void *, BC_Function *> callbacks;

        Array<BC_Function *> functions = {};
        Array<BC_Function *> foreign_functions = {};

        int64_t exit_code = 0;
    };

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data);
    void interpreter_free(Interpreter *interp);

    void interpreter_start(Interpreter *interp, BC_Function *entry_func,
                           Array<BC_Global_Info> global_info, int64_t global_size,
                           Array<BC_Function *>functions,
                           Array<BC_Function *> foreign_functions);
    void interpreter_start(Interpreter *interp, BC_Function *entry_funcm,
                           int64_t first_temp_index = 1,// @TODO: FIXME: CLEANUP: Why o why would this be 1???
                           int64_t return_value_index = 0);

    Interpreter_Value interp_load_value(Interpreter *interp, BC_Value *bc_val);
    Interpreter_LValue interp_load_lvalue(Interpreter *interp, BC_Value *bc_val);

    void interp_store(Interpreter *interp, Interpreter_Value source, Interpreter_LValue dest);
    void interp_store(Interpreter *interp, Interpreter_Value source, void *dest_ptr,
                      AST_Type *dest_type);
    void interp_store(Interpreter *interp, void *source_ptr, AST_Type *source_type,
                      Interpreter_LValue dest, bool allow_type_mismatch = false);

    void interpreter_initialize_globals(Interpreter *interp, Array<BC_Global_Info> global_info,
                                        int64_t global_data_size);

    void interp_store_constant(Interpreter *interp, Const_Value const_val, Interpreter_LValue dest);
    void interp_store_constant(Interpreter *interp, Const_Value const_val, void *dest_ptr);

    void interpreter_initialize_foreigns(Interpreter *interp,
                                         Array<BC_Function *> foreign_functions);

    void interpreter_execute_foreign_function(Interpreter *interp, BC_Function *bc_func,
                                              int64_t arg_count, BC_Value *result_value);
    void interpreter_call_function_pointer(Interpreter *interp, void *fn_ptr, AST_Type *fn_type,
                                           int64_t arg_count, BC_Value *result_value);

    void interpreter_execute_compiler_function(Interpreter *interp, BC_Function *func,
                                               int64_t arg_count, Interpreter_LValue *result_lvalue);
}
