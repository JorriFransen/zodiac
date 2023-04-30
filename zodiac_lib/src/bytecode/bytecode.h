#pragma once

#include "atom.h"
#include "common.h"
#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac {

struct Allocator;
struct Type;
struct Zodiac_Context;

namespace Bytecode {

#define ZODIAC_BC_OPS \
    ZODIAC_BC_OP(NOP) \
    ZODIAC_BC_OP(I_ADD) \
    ZODIAC_BC_OP(I_SUB) \
    ZODIAC_BC_OP(I_MUL) \
    ZODIAC_BC_OP(I_DIV) \
    ZODIAC_BC_OP(I_EQ) \
    ZODIAC_BC_OP(I_NEQ) \
    ZODIAC_BC_OP(I_GT) \
    ZODIAC_BC_OP(I_LT) \
    ZODIAC_BC_OP(I_GT_EQ) \
    ZODIAC_BC_OP(I_LT_EQ) \
    ZODIAC_BC_OP(F_ADD) \
    ZODIAC_BC_OP(F_SUB) \
    ZODIAC_BC_OP(F_MUL) \
    ZODIAC_BC_OP(F_DIV) \
    ZODIAC_BC_OP(F_EQ) \
    ZODIAC_BC_OP(F_NEQ) \
    ZODIAC_BC_OP(F_GT) \
    ZODIAC_BC_OP(F_LT) \
    ZODIAC_BC_OP(F_GT_EQ) \
    ZODIAC_BC_OP(F_LT_EQ) \
    ZODIAC_BC_OP(SQRT) \
    ZODIAC_BC_OP(TRUNC) \
    ZODIAC_BC_OP(PRINT) \
    ZODIAC_BC_OP(PUSH_ARG) \
    ZODIAC_BC_OP(CALL) \
    ZODIAC_BC_OP(CALL_FOREIGN) \
    ZODIAC_BC_OP(CALL_PTR) \
    ZODIAC_BC_OP(RETURN_VOID) \
    ZODIAC_BC_OP(RETURN) \
    ZODIAC_BC_OP(ALLOC) \
    ZODIAC_BC_OP(ADDROF_ALLOC) \
    ZODIAC_BC_OP(ADDROF_FUNC) \
    ZODIAC_BC_OP(LOAD_G) \
    ZODIAC_BC_OP(STORE_G) \
    ZODIAC_BC_OP(STORE_A) \
    ZODIAC_BC_OP(LOAD_A) \
    ZODIAC_BC_OP(STORE_PTR) \
    ZODIAC_BC_OP(LOAD_PTR) \
    ZODIAC_BC_OP(INSERT_VALUE) \
    ZODIAC_BC_OP(EXTRACT_VALUE) \
    ZODIAC_BC_OP(INSERT_ELEMENT) \
    ZODIAC_BC_OP(EXTRACT_ELEMENT) \
    ZODIAC_BC_OP(AGG_OFFSET_POINTER) \
    ZODIAC_BC_OP(ARR_OFFSET_POINTER) \
    ZODIAC_BC_OP(JMP) \
    ZODIAC_BC_OP(JMP_IF) \
    ZODIAC_BC_OP(PHI) \


#define ZODIAC_BC_OP(op) op,
enum class Bytecode_Opcode
{
    ZODIAC_BC_OPS

    FIRST = NOP,
    LAST = PHI,
};
#undef ZODIAC_BC_OP

extern const char *Bytecode_Opecode_Names[(u64)Bytecode_Opcode::LAST + 1];

enum class Bytecode_Register_Kind
{
    INVALID,
    TEMPORARY,
    FUNCTION,
    BLOCK,
    ALLOC,
    TYPE,
    GLOBAL,
    PHI_ARGS,
    UNDEF,
};

typedef u64 Bytecode_Register_Flags;
enum Bytecode_Register_Flags_ : Bytecode_Register_Flags
{
    BC_REGISTER_FLAG_NONE      = 0x000,
    BC_REGISTER_FLAG_LITERAL   = 0x001,
    BC_REGISTER_FLAG_ARGUMENT  = 0x002,
    BC_REGISTER_FLAG_CONSTANT  = 0x004,
};

typedef s64 Bytecode_Function_Handle;
typedef s64 Bytecode_Global_Handle;
typedef s64 Bytecode_Block_Handle;
typedef s64 Bytecode_Phi_Args_Handle;
typedef s64 Bytecode_Global_Handle;

union Bytecode_Register_Value
{
    Integer_Value integer;
    Real_Value real;
    bool boolean;
    u8 *pointer;

    Bytecode_Function_Handle function_handle;
};

struct Bytecode_Register
{
    Bytecode_Register_Kind kind = Bytecode_Register_Kind::INVALID;
    Bytecode_Register_Flags flags = BC_REGISTER_FLAG_NONE;
    s64  index = -1;
    Type *type = nullptr;

    union {
        Bytecode_Register_Value value = {};
        Bytecode_Block_Handle block_handle;
        Bytecode_Phi_Args_Handle phi_args_handle;
        const char *alloc_name;
    };
};

struct Bytecode_Instruction
{
    Bytecode_Opcode op = Bytecode_Opcode::NOP;

    Bytecode_Register a = {};
    Bytecode_Register b = {};
    Bytecode_Register dest = {};

    s64 additional_index = 0;
};

struct Bytecode_Block
{
    Atom name = {};
    Dynamic_Array<Bytecode_Instruction> instructions = {};
};

struct Bytecode_Instruction_Handle
{
    Bytecode_Function_Handle fn_index = -1;
    Bytecode_Block_Handle block_index = -1;
    s64 instruction_index = -1;
};

ZAPI const bool operator==(const Bytecode_Instruction_Handle &lhs, const Bytecode_Instruction_Handle &rhs);
ZAPI const bool operator!=(const Bytecode_Instruction_Handle &lhs, const Bytecode_Instruction_Handle &rhs);

struct Bytecode_Phi_Args
{
    Bytecode_Register true_value = {};
    Bytecode_Register false_value = {};
    Bytecode_Block_Handle true_block_handle = 0;
    Bytecode_Block_Handle false_block_handle = 0;
};

typedef int BC_Function_Flag;
enum BC_Function_Flag_ : BC_Function_Flag
{
    BC_FUNCTION_FLAG_NONE     = 0x000,
    BC_FUNCTION_FLAG_FOREIGN  = 0x001,
    BC_FUNCTION_FLAG_NORETURN = 0x002,
};

struct Bytecode_Function
{
    BC_Function_Flag flags = BC_FUNCTION_FLAG_NONE;

    Atom name = {};
    Type *type = nullptr;

    Dynamic_Array<Bytecode_Register> registers = {};
    Dynamic_Array<Bytecode_Block> blocks = {};
    Dynamic_Array<Bytecode_Phi_Args> phi_args = {};

    s64 required_stack_size = 0;

    // TODO
    // FFI_Handle ffi_handle = nullptr;
};

struct Bytecode_Global
{
    Atom atom = {};
    Type *type = nullptr;
    bool constant = false;
    Bytecode_Register initial_value = {};
};

struct Bytecode_Program
{
    Bytecode_Function_Handle entry_handle = -1;

    Array_Ref<Bytecode_Function> functions = {};
    Array_Ref<Bytecode_Function_Handle> foreign_functions = {};

    Array_Ref<Bytecode_Global> globals = {};
    s64 required_global_size = -1;
};

struct BC_Global_Reg__
{
    Atom atom;
    Bytecode_Register reg;
};

struct Bytecode_Builder
{
    Allocator *allocator = nullptr;
    Zodiac_Context *zodiac_context = nullptr;

    Dynamic_Array<Bytecode_Function> functions = {};
    Dynamic_Array<Bytecode_Function_Handle> foreign_functions = {};

    Dynamic_Array<Bytecode_Global> globals = {};
    s64 required_global_size = 0;

    // TODO
    // Hash_Table<Atom, Bytecode_Register> global_registers = {};
    Dynamic_Array<BC_Global_Reg__> global_registers;

    s64 insert_fn_index = -1;
    s32 insert_block_index = -1;
};

ZAPI Bytecode_Builder bytecode_builder_create(Allocator *allocator, Zodiac_Context *cu);
ZAPI void bytecode_builder_free(Bytecode_Builder *bb);

ZAPI Bytecode_Program bytecode_get_program(Bytecode_Builder *builder);

ZAPI Bytecode_Function_Handle bytecode_function_create(Bytecode_Builder *builder, const char* cstr_fn_name, Type *fn_type, BC_Function_Flag flags = BC_FUNCTION_FLAG_NONE);
ZAPI Bytecode_Function_Handle bytecode_function_create(Bytecode_Builder *builder, Atom fn_name, Type *fn_type, BC_Function_Flag flags = BC_FUNCTION_FLAG_NONE);

ZAPI Bytecode_Function_Handle bytecode_foreign_function_create(Bytecode_Builder *builder, const char *cstr_fn_name, Type *fn_type);
ZAPI Bytecode_Function_Handle bytecode_foreign_function_create(Bytecode_Builder *builder, Atom name, Type *fn_type);

ZAPI Bytecode_Global_Handle bytecode_create_global(Bytecode_Builder *builder, const char *cstr_name, Type *type, Bytecode_Register initial_value = {});
ZAPI Bytecode_Global_Handle bytecode_create_global(Bytecode_Builder *builder, Atom name, Type *type, Bytecode_Register initial_value = {});

ZAPI Bytecode_Block_Handle bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, const char* cstr_name);
ZAPI Bytecode_Block_Handle bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, Atom name);

ZAPI void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, Bytecode_Block_Handle block_handle);

ZAPI Bytecode_Register bytecode_integer_literal(Bytecode_Builder *builder, Type *type, s64 value);
ZAPI Bytecode_Register bytecode_integer_literal(Bytecode_Builder *builder, Type *type, Integer_Value iv);
ZAPI Bytecode_Register bytecode_real_literal(Bytecode_Builder *builder, Type *type, float float_value, double double_value);
ZAPI Bytecode_Register bytecode_real_literal(Bytecode_Builder *builder, Type *type, Real_Value rv);
ZAPI Bytecode_Register bytecode_boolean_literal(Bytecode_Builder *builder, Type *type, bool value);
ZAPI Bytecode_Register bytecode_block_value(Bytecode_Builder *builder, Bytecode_Block_Handle block_handle);
ZAPI Bytecode_Register bytecode_type_value(Bytecode_Builder *builder, Type *type);
ZAPI Bytecode_Register bytecode_register_create(Bytecode_Builder *builder, Bytecode_Register_Kind kind, Type *type, Bytecode_Register_Flags flags = BC_REGISTER_FLAG_NONE);
ZAPI Bytecode_Phi_Args_Handle bytecode_phi_arg_create(Bytecode_Builder *builder, Bytecode_Register true_val, Bytecode_Block_Handle true_block, Bytecode_Register false_val, Bytecode_Block_Handle false_block);

ZAPI Bytecode_Register bytecode_load_argument(Bytecode_Builder *builder, s64 index);

ZAPI Bytecode_Register bytecode_emit_add(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_sub(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_mul(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_div(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);

ZAPI Bytecode_Register bytecode_emit_eq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_neq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_gt(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_lt(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_gteq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);
ZAPI Bytecode_Register bytecode_emit_lteq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b);

ZAPI Bytecode_Register bytecode_emit_sqrt(Bytecode_Builder *builder, Bytecode_Register a);

ZAPI Bytecode_Register bytecode_emit_cast(Bytecode_Builder *builder, Type *target_type, Bytecode_Register operand_register);
ZAPI Bytecode_Register bytecode_emit_integer_cast(Bytecode_Builder *builder, Type *target_type, Bytecode_Register operand_register);

ZAPI void bytecode_emit_print(Bytecode_Builder *builder, Bytecode_Register a);

ZAPI void bytecode_emit_push_arg(Bytecode_Builder *builder, Bytecode_Register arg_register);

ZAPI Bytecode_Register bytecode_emit_call(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, Bytecode_Register arg_count_register);
ZAPI Bytecode_Register bytecode_emit_call(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, s64 arg_count);

ZAPI Bytecode_Register bytecode_emit_call_pointer(Bytecode_Builder *builder, Bytecode_Register fn_ptr_reg, Bytecode_Register arg_count_register);
ZAPI Bytecode_Register bytecode_emit_call_pointer(Bytecode_Builder *builder, Bytecode_Register fn_ptr_reg, s64 arg_count);

ZAPI void bytecode_emit_return(Bytecode_Builder *builder);
ZAPI void bytecode_emit_return(Bytecode_Builder *builder, Bytecode_Register return_value);

ZAPI Bytecode_Register bytecode_emit_alloc(Bytecode_Builder *builder, Type *type, const char *name);
ZAPI Bytecode_Register bytecode_emit_address_of_alloc(Bytecode_Builder *builder, Bytecode_Register alloc);
ZAPI Bytecode_Register bytecode_emit_address_of_function(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle);

ZAPI void bytecode_emit_store_global(Bytecode_Builder *builder, Bytecode_Register source, Bytecode_Global_Handle global_handle);
ZAPI Bytecode_Register bytecode_emit_load_global(Bytecode_Builder *builder, Bytecode_Global_Handle global_handle);

ZAPI void bytecode_emit_store_alloc(Bytecode_Builder *builder, Bytecode_Register source, Bytecode_Register dest);
ZAPI Bytecode_Register bytecode_emit_load_alloc(Bytecode_Builder *builder, Bytecode_Register source);

ZAPI void bytecode_emit_store_pointer(Bytecode_Builder *builder, Bytecode_Register source, Bytecode_Register dest);
ZAPI Bytecode_Register bytecode_emit_load_pointer(Bytecode_Builder *builder, Bytecode_Register source);

ZAPI Bytecode_Register bytecode_emit_insert_value(Bytecode_Builder *builder, Bytecode_Register aggregate, Bytecode_Register new_elem_val, Type *struct_type, s64 index);
ZAPI Bytecode_Register bytecode_emit_extract_value(Bytecode_Builder *builder, Bytecode_Register aggregate, s64 index);

ZAPI Bytecode_Register bytecode_emit_insert_element(Bytecode_Builder *builder, Bytecode_Register array, Bytecode_Register new_elem_val, Type *array_type, s64 index);
ZAPI Bytecode_Register bytecode_emit_extract_element(Bytecode_Builder *builder, Bytecode_Register array, s64 index);

ZAPI Bytecode_Register bytecode_emit_aggregate_offset_pointer(Bytecode_Builder *builder, Bytecode_Register agg_register, s64 index);
ZAPI Bytecode_Register bytecode_emit_array_offset_pointer(Bytecode_Builder *builder, Bytecode_Register array_register, s64 index);

ZAPI void bytecode_emit_jmp(Bytecode_Builder *builder, Bytecode_Block_Handle block);
ZAPI void bytecode_emit_jmp_if(Bytecode_Builder *builder, Bytecode_Register cond, Bytecode_Block_Handle then_block, Bytecode_Block_Handle else_block);

ZAPI Bytecode_Instruction_Handle bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Opcode op, Bytecode_Register a, Bytecode_Register b, Bytecode_Register result);
ZAPI Bytecode_Instruction *bytecode_get_instruction(Bytecode_Builder *bb, const Bytecode_Instruction_Handle &handle);

ZAPI Bytecode_Register bytecode_retype_literal(Type *target_type, Bytecode_Register operand_register);

} }