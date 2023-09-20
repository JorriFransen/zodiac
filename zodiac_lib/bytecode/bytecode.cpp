#include "bytecode.h"

#include "memory/temporary_allocator.h"
#include "type.h"
#include "util/asserts.h"
#include "util/zstring.h"
#include "zodiac_context.h"

namespace Zodiac { namespace Bytecode {

#define ZODIAC_BC_OP(op) #op,
const char *Bytecode_Opecode_Names[] = {
    ZODIAC_BC_OPS
};
#undef ZODIAC_BC_OP

const bool operator==(const Bytecode_Instruction_Handle &lhs, const Bytecode_Instruction_Handle &rhs) {
    return (lhs.fn_index == rhs.fn_index) &&
           (lhs.block_index == rhs.block_index) &&
           (lhs.instruction_index == rhs.instruction_index);
}

const bool operator!=(const Bytecode_Instruction_Handle &lhs, const Bytecode_Instruction_Handle &rhs) {
    return !operator==(lhs, rhs);
}

u64 hash_key(Bytecode_Instruction_Handle handle)
{
    auto h1 = hash_s64((s64)handle.fn_index);
    auto h2 = hash_s64((s64)handle.block_index);
    auto h3 = hash_s64((s64)handle.instruction_index);

    return hash_mix(hash_mix(h1, h2), h3);
}

Bytecode_Builder bytecode_builder_create(Allocator *allocator, Zodiac_Context *cu)
{
    Bytecode_Builder result;
    bytecode_builder_init(allocator, cu, &result);
    return result;
}

void bytecode_builder_init(Allocator *bytecode_allocator, Zodiac_Context *cu, Bytecode_Builder *out_builder)
{
    debug_assert(bytecode_allocator && cu && out_builder);

    out_builder->bytecode_allocator = bytecode_allocator,
    out_builder->zodiac_context = cu,
    out_builder->required_global_size = 0,
    out_builder->insert_fn_index = -1,
    out_builder->insert_block_index = -1,

    dynamic_array_create(bytecode_allocator, &out_builder->functions);
    dynamic_array_create(bytecode_allocator, &out_builder->foreign_functions);
    dynamic_array_create(bytecode_allocator, &out_builder->globals);

    hash_table_create(bytecode_allocator, &out_builder->global_registers);
}

void bytecode_builder_free(Bytecode_Builder *bb)
{
    dynamic_array_free(&bb->functions);
    dynamic_array_free(&bb->foreign_functions);
    dynamic_array_free(&bb->globals);
}

Bytecode_Program bytecode_get_program(Bytecode_Builder *builder)
{
    Bytecode_Program result = {
        .functions = builder->functions,
        .foreign_functions = builder->foreign_functions,
        .globals = builder->globals,
        .required_global_size = builder->required_global_size,
    };

    return result;
}

Bytecode_Function_Handle bytecode_find_entry(Bytecode_Program program)
{
    debug_assert(program.entry_handle == -1);

    for (s64 i = 0; i < program.functions.count; i++) {

        auto fn = &program.functions[i];
        if (fn->name == "main") {
            return (Bytecode_Function_Handle)i;
        }
    }

    assert_msg(false, "Entry point not found");
    return -1;
}

bool bytecode_instruction_is_terminator(Bytecode_Instruction *inst)
{
    debug_assert(inst);

    auto op = inst->op;

    return op == Bytecode_Opcode::RETURN      ||
           op == Bytecode_Opcode::RETURN_VOID ||
           op == Bytecode_Opcode::JMP         ||
           op == Bytecode_Opcode::JMP_IF;
}

bool bytecode_block_is_terminated(Bytecode_Block *block)
{
    debug_assert(block);

    if (block->instructions.count) {
        return block->terminated;
    }

    return false;
}

bool bytecode_block_is_terminated(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, Bytecode_Block_Handle block_handle)
{
    debug_assert(builder);

    assert(fn_handle >= 0 && fn_handle < builder->functions.count);

    auto fn = &builder->functions[fn_handle];

    assert(block_handle >= 0 && block_handle < fn->blocks.count);

    return bytecode_block_is_terminated(&fn->blocks[block_handle]);
}

Bytecode_Function_Handle bytecode_function_create(Bytecode_Builder *builder, const char* cstr_fn_name, Type *fn_type, BC_Function_Flag flags/*=BC_FUNCTION_FLAG_NONE*/)
{
    Atom atom = atom_get(&builder->zodiac_context->atoms, cstr_fn_name);
    return bytecode_function_create(builder, atom, fn_type, flags);
}

Bytecode_Function_Handle bytecode_function_create(Bytecode_Builder *builder, Atom fn_name, Type *fn_type, BC_Function_Flag flags/*=BC_FUNCTION_FLAG_NONE*/)
{
    auto index = builder->functions.count;

    debug_assert(fn_type->kind == Type_Kind::FUNCTION);

    for (s64 i = 0; i < builder->functions.count; i++) {
        if (builder->functions[i].name == fn_name) {
            assert_msg(false, "Function with this name already exists");
            return -1;
        }
    }

    bool is_foreign = flags & BC_FUNCTION_FLAG_FOREIGN;

    if (is_foreign) {
        assert(!(fn_type->function.return_type->flags & TYPE_FLAG_AGGREGATE) && "Foreign functions cannot have aggregate return types");

#ifndef NDEBUG
        for (s64 i = 0; i < fn_type->function.parameter_types.count; i++) {
            auto arg_type = fn_type->function.parameter_types[i];
            assert(!(arg_type->flags & TYPE_FLAG_AGGREGATE) && "Foreign functions cannot have aggregate argument types!");
        }
#endif
    }

    Dynamic_Array<Bytecode_Register> registers = {};
    Dynamic_Array<Bytecode_Block> blocks = {};
    Dynamic_Array<Bytecode_Phi_Args> phi_args = {};


    if (!is_foreign) {
        dynamic_array_create(builder->bytecode_allocator, &registers);
        dynamic_array_create(builder->bytecode_allocator, &blocks);
        dynamic_array_create(builder->bytecode_allocator, &phi_args);
    }

    Bytecode_Function result = {
        .flags = flags,
        .name = fn_name,
        .type = fn_type,
        .registers = registers,
        .blocks = blocks,
        .phi_args = phi_args,
        .required_stack_size = 0,
    };

    if (!is_foreign) {
        for (s64 i = 0; i < fn_type->function.parameter_types.count; i++) {

            auto arg_index = result.registers.count;

            Bytecode_Register arg_register = {
                .kind = Bytecode_Register_Kind::TEMPORARY,
                .flags = BC_REGISTER_FLAG_ARGUMENT,
                .index = (s64)arg_index,
                .type = fn_type->function.parameter_types[i],
            };
            dynamic_array_append(&result.registers, arg_register);
        }


    } else {
        dynamic_array_append(&builder->foreign_functions, (Bytecode_Function_Handle)index);
    }

    dynamic_array_append(&builder->functions, result);
    auto fn_index = builder->functions.count - 1;

    auto reg = bytecode_register_create(builder, Bytecode_Register_Kind::FUNCTION, fn_type);
    reg.value.function_handle = fn_index;

    hash_table_add(&builder->global_registers, fn_name, reg);

    return index;
}

Bytecode_Function_Handle bytecode_foreign_function_create(Bytecode_Builder *builder, const char *cstr_fn_name, Type *fn_type)
{
    Atom atom = atom_get(&builder->zodiac_context->atoms, cstr_fn_name);
    return bytecode_foreign_function_create(builder, atom, fn_type);
}

Bytecode_Function_Handle bytecode_foreign_function_create(Bytecode_Builder *builder, Atom name, Type *fn_type)
{
    return bytecode_function_create(builder, name, fn_type, BC_FUNCTION_FLAG_FOREIGN);
}

Bytecode_Global_Handle bytecode_create_global(Bytecode_Builder *builder, const char *cstr_name, Type *type, bool constant, Bytecode_Register initial_value /*={}*/)
{
    Atom atom = atom_get(&builder->zodiac_context->atoms, cstr_name);
    return bytecode_create_global(builder, atom, type, constant, initial_value);
}

Bytecode_Global_Handle bytecode_create_global(Bytecode_Builder *builder, Atom name, Type *type, bool constant, Bytecode_Register initial_value /*={}*/)
{
    if (initial_value.kind != Bytecode_Register_Kind::INVALID) {
        assert(initial_value.flags & BC_REGISTER_FLAG_CONSTANT);
    }

    Bytecode_Global global_var = {
        .atom = name,
        .type = type,
        .constant = constant,
        .initial_value = initial_value,
    };

    // @Cleanup: @TODO: @FIXME: alignment
    assert(type->bit_size % 8 == 0);
    auto size = type->bit_size / 8;
    builder->required_global_size += size;

    dynamic_array_append(&builder->globals, global_var);
    auto index = builder->globals.count - 1;

    auto global_reg = bytecode_register_create(builder, Bytecode_Register_Kind::GLOBAL, type);
    global_reg.index = index;

    hash_table_add(&builder->global_registers, name, global_reg);

    return (Bytecode_Global_Handle)index;
}

Bytecode_Block_Handle bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, const char* cstr_name)
{
    Atom atom = atom_get(&builder->zodiac_context->atoms, cstr_name);
    return bytecode_append_block(builder, fn_handle, atom);
}

Bytecode_Block_Handle bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, Atom name)
{
    assert(fn_handle >= 0 && fn_handle < builder->functions.count);
    auto fn = &builder->functions[fn_handle];

    auto index = fn->blocks.count;

    bool duplicate = false;
    for (s64 i = 0; i < fn->blocks.count; i++) {
        if (fn->blocks[i].name == name) {
            duplicate = true;
            break;
        }
    }

    if (duplicate) {

        char new_name[ZSTRING_FORMAT_STACK_BUFFER_SIZE];
        s64 new_length = 0;

        s64 index = 0;
        while (duplicate) {

            new_length = string_format(new_name, "%s_%i", name.data, index + 1);
            assert(new_length + 1 <= ZSTRING_FORMAT_STACK_BUFFER_SIZE);

            duplicate = false;
            for (s64 i = index; i < fn->blocks.count; i++) {
                if (fn->blocks[i].name == new_name) {
                    duplicate = true;
                    break;
                }
            }

            index += 1;
        }

        name = atom_get(&builder->zodiac_context->atoms, { new_name, new_length } );
    }

    Bytecode_Block block = {
        .name = name,
        .terminated = false,
    };

    dynamic_array_create(builder->bytecode_allocator, &block.instructions);

    dynamic_array_append(&fn->blocks, block);

    return index;
}

void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle, Bytecode_Block_Handle block_handle)
{
    assert(fn_handle >= 0 && fn_handle < builder->functions.count);
#ifndef NDEBUG
    auto fn = &builder->functions[fn_handle];
#endif
    assert(block_handle >= 0 && block_handle < fn->blocks.count);

    builder->insert_fn_index = fn_handle;
    builder->insert_block_index = block_handle;
}

Bytecode_Block *bytecode_get_insert_block(Bytecode_Builder *builder)
{
    debug_assert(builder);

    assert(builder->insert_fn_index >= 0 && builder->insert_fn_index < builder->functions.count);

    auto func = builder->functions[builder->insert_fn_index];

    assert(builder->insert_block_index >= 0 && builder->insert_block_index < func.blocks.count);

    return &func.blocks[builder->insert_block_index];
}

Bytecode_Register bytecode_integer_literal(Bytecode_Builder *builder, Type *type, s64 value)
{
    return bytecode_integer_literal(builder, type, { .s64 = value });
}

Bytecode_Register bytecode_integer_literal(Bytecode_Builder *builder, Type *type, Integer_Value iv)
{
    debug_assert(builder);
    assert(type->kind == Type_Kind::INTEGER);

    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);
    result.value.integer = iv;

    return result;
}

Bytecode_Register bytecode_pointer_literal(Bytecode_Builder *builder, Type *type, void *ptr)
{
    debug_assert(builder);
    assert(type->kind == Type_Kind::POINTER);

    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);
    result.value.pointer = (u8*)ptr;

    return result;

}

Bytecode_Register bytecode_real_literal(Bytecode_Builder *builder, Type *type, float float_value, double double_value)
{
    return bytecode_real_literal(builder, type, { .r32 = float_value, .r64 = double_value });
}

Bytecode_Register bytecode_real_literal(Bytecode_Builder *builder, Type *type, Real_Value rv)
{
    assert(type->kind == Type_Kind::FLOAT);
    assert(type->bit_size == 32 || type->bit_size == 64);

    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);

    result.value.real = rv;

    return result;
}

Bytecode_Register bytecode_boolean_literal(Bytecode_Builder *builder, Type *type, bool value)
{
    assert(type->kind == Type_Kind::BOOLEAN);
    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);

    result.value.boolean = value;

    return result;
}

Bytecode_Register bytecode_string_literal(Bytecode_Builder *bb, String_Ref str)
{
    Dynamic_Array<Bytecode_Register> values;
    dynamic_array_create<Bytecode_Register>(bb->bytecode_allocator, &values, 2);

    auto u8_ptr_type = get_pointer_type(&builtin_type_u8, &bb->zodiac_context->ast_allocator);

    auto string_memory_register = bytecode_register_create(bb, Bytecode_Register_Kind::TEMPORARY, u8_ptr_type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);
    string_memory_register.value.string = string_create(str);

    dynamic_array_append(&values, string_memory_register);
    dynamic_array_append(&values, bytecode_integer_literal(bb, &builtin_type_s64, str.length));

    return bytecode_aggregate_literal(bb, values, get_string_type(bb->zodiac_context));
}

Bytecode_Register bytecode_aggregate_literal(Bytecode_Builder *bb, Dynamic_Array<Bytecode_Register> members, Type *type)
{
    debug_assert(bb && members.count && type);
    assert(type->flags & TYPE_FLAG_AGGREGATE);
    assert(type->kind == Type_Kind::STRUCTURE);

    bool all_literal = true;
    bool all_const = true;

    for (s64 i = 0; i < members.count; i++) {

        if (!(members[i].flags & BC_REGISTER_FLAG_LITERAL)) {
            all_literal = false;
        }

        if (!(members[i].flags & BC_REGISTER_FLAG_CONSTANT)) {
            all_const = false;
        }

        if (!all_literal && !all_const) {
            break;
        }
    }

    Bytecode_Register_Flags flags = BC_REGISTER_FLAG_NONE;
    if (all_literal) {
        flags |= BC_REGISTER_FLAG_LITERAL;
    }
    if (all_const) {
        flags |= BC_REGISTER_FLAG_CONSTANT;
    }

    auto result = bytecode_register_create(bb, Bytecode_Register_Kind::TEMPORARY, type, flags);

    result.value.compound = members;

    return result;
}

Bytecode_Register bytecode_array_literal(Bytecode_Builder *bb, Dynamic_Array<Bytecode_Register> values, Type *type)
{
    debug_assert(bb && values.count && type);
    assert(type->kind == Type_Kind::STATIC_ARRAY);

#ifndef NDEBUG
    for (s64 i = 0; i < values.count; i++) {
        debug_assert(values[i].flags & BC_REGISTER_FLAG_LITERAL);
        debug_assert(values[i].flags & BC_REGISTER_FLAG_CONSTANT);
    }
#endif // NDEBUG

    auto result = bytecode_register_create(bb, Bytecode_Register_Kind::TEMPORARY, type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);

    result.value.compound = values;

    return result;
}

Bytecode_Register bytecode_block_value(Bytecode_Builder *builder, Bytecode_Block_Handle block_handle)
{
    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::BLOCK, nullptr, BC_REGISTER_FLAG_NONE);
    result.block_handle = block_handle;

    return result;
}

Bytecode_Register bytecode_type_value(Bytecode_Builder *builder, Type *type)
{
    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TYPE, type, BC_REGISTER_FLAG_NONE);
    result.type = type;
    return result;
}

Bytecode_Register bytecode_register_create(Bytecode_Builder *builder, Bytecode_Register_Kind kind, Type *type, Bytecode_Register_Flags flags /*=BC_REGISTER_FLAG_NONE*/, const char *alloc_name/*=nullptr*/)
{
    debug_assert(builder);
    assert(kind != Bytecode_Register_Kind::INVALID);

    bool literal = flags & BC_REGISTER_FLAG_LITERAL;
    bool global = kind == Bytecode_Register_Kind::GLOBAL || kind == Bytecode_Register_Kind::FUNCTION;

    Bytecode_Register result = {
        .kind = kind,
        .flags = flags,
        .index = -1,
        .type = type,
    };

    if (alloc_name) {
        assert(kind == Bytecode_Register_Kind::ALLOC);
        result.alloc_name = alloc_name;
    }

    if (!literal && !global) {
        assert(builder->insert_fn_index >= 0 &&
               builder->insert_fn_index < builder->functions.count);
        auto fn = &builder->functions[builder->insert_fn_index];
        assert(builder->insert_block_index >= 0 &&
               builder->insert_block_index < fn->blocks.count);


        if (kind == Bytecode_Register_Kind::TEMPORARY ||
             kind == Bytecode_Register_Kind::ALLOC) {
            result.index = fn->registers.count;
            assert(result.index < I64_MAX);
            assert(result.index >= 0);
        }

        if (result.index >= 0) dynamic_array_append(&fn->registers, result);
    }

    return result;
}

Bytecode_Phi_Args_Handle bytecode_phi_arg_create(Bytecode_Builder *builder, Bytecode_Register true_val, Bytecode_Block_Handle true_block, Bytecode_Register false_val, Bytecode_Block_Handle false_block)
{
    assert(builder->insert_fn_index >= 0 &&
           builder->insert_fn_index < builder->functions.count);
    auto fn = &builder->functions[builder->insert_fn_index];

    Bytecode_Phi_Args_Handle handle = fn->phi_args.count;

    Bytecode_Phi_Args phi_args = { true_val, false_val, true_block, false_block };
    dynamic_array_append(&fn->phi_args, phi_args);

    return handle;
}

Bytecode_Register bytecode_emit_load_argument(Bytecode_Builder *builder, s64 index)
{
    auto fn_handle = builder->insert_fn_index;
    assert(fn_handle >= 0 && fn_handle < builder->functions.count);
    auto fn = &builder->functions[fn_handle];

    assert(fn->type->function.is_vararg == false);
    assert(fn->type->function.parameter_types.count > index);

    auto result = fn->registers[index];
    assert(result.index == index);
    assert(result.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(result.flags & BC_REGISTER_FLAG_ARGUMENT);

    return result;
}

#define EMIT_BINOP_(op) \
    assert(a.kind == Bytecode_Register_Kind::TEMPORARY); \
    assert(b.kind == Bytecode_Register_Kind::TEMPORARY); \
    assert(a.type == b.type && "EMIT_BINOP_ register types don't match"); \
    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, a.type); \
    if (a.type->kind == Type_Kind::INTEGER) { \
        bytecode_emit_instruction(builder, Bytecode_Opcode::I_##op, a, b, result); \
        return result; \
    }  else if (a.type->kind == Type_Kind::FLOAT) { \
        bytecode_emit_instruction(builder, Bytecode_Opcode::F_##op, a, b, result); \
        return result; \
    } else { \
        assert_msg(false, "EMIT_BINOP_ unsupported binop type"); \
        return {}; \
    }

Bytecode_Register bytecode_emit_add(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_BINOP_(ADD)
}

Bytecode_Register bytecode_emit_sub(Bytecode_Builder *builder, Bytecode_Register a,
                                    Bytecode_Register b)
{
    EMIT_BINOP_(SUB)
}

Bytecode_Register bytecode_emit_mul(Bytecode_Builder *builder, Bytecode_Register a,
                                    Bytecode_Register b)
{
    EMIT_BINOP_(MUL)
}

Bytecode_Register bytecode_emit_div(Bytecode_Builder *builder, Bytecode_Register a,
                                    Bytecode_Register b)
{
    EMIT_BINOP_(DIV)
}

#undef EMIT_INTEGER_BINOP_

#define EMIT_CMP_BINOP_(op) { \
    assert(a.kind == Bytecode_Register_Kind::TEMPORARY);\
    assert(b.kind == Bytecode_Register_Kind::TEMPORARY);\
    assert(a.type == b.type); \
    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, &builtin_type_bool); \
    if (a.type->kind == Zodiac::Type_Kind::INTEGER) { \
        bytecode_emit_instruction(builder, Bytecode_Opcode::I_##op, a, b, result); \
    } else if (a.type->kind == Type_Kind::FLOAT) { \
        bytecode_emit_instruction(builder, Bytecode_Opcode::F_##op, a, b, result); \
    } else if (a.type->kind == Type_Kind::BOOLEAN) { \
        bytecode_emit_instruction(builder, Bytecode_Opcode::I_##op, a, b, result); \
    } else { \
        assert(false && !"EMIT_CMP_BINOP unhandled type"); \
    }\
    return result; \
}

Bytecode_Register bytecode_emit_eq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_CMP_BINOP_(EQ);
}

Bytecode_Register bytecode_emit_neq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_CMP_BINOP_(NEQ);
}

Bytecode_Register bytecode_emit_gt(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_CMP_BINOP_(GT);
}

Bytecode_Register bytecode_emit_lt(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_CMP_BINOP_(LT);
}

Bytecode_Register bytecode_emit_gteq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_CMP_BINOP_(GT_EQ);
}

Bytecode_Register bytecode_emit_lteq(Bytecode_Builder *builder, Bytecode_Register a, Bytecode_Register b)
{
    EMIT_CMP_BINOP_(LT_EQ);
}

Bytecode_Register bytecode_emit_sqrt(Bytecode_Builder *builder, Bytecode_Register operand)
{
    assert(operand.kind == Bytecode_Register_Kind::TEMPORARY);

    auto result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, operand.type);
    bytecode_emit_instruction(builder, Bytecode_Opcode::SQRT, operand, {}, result);
    return result;
}

Bytecode_Register bytecode_emit_cast(Bytecode_Builder *builder, Type *target_type, Bytecode_Register operand_register)
{
    auto op_type = operand_register.type;

    if (op_type == target_type) {
        return operand_register;
    }

    switch (target_type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            if (operand_register.flags & BC_REGISTER_FLAG_LITERAL) {
                return bytecode_retype_literal(target_type, operand_register);
            }
            return bytecode_emit_integer_cast(builder, target_type, operand_register);
            break;
        }

        case Type_Kind::FLOAT: assert(false); break;
        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
        case Type_Kind::BOOLEAN: assert(false); break;
        case Type_Kind::STRUCTURE: assert(false); break;

        case Type_Kind::STATIC_ARRAY: {
            assert(false);
            break;
        }
    }

    assert(false);
    return { .kind = Bytecode_Register_Kind::INVALID };
}

Bytecode_Register bytecode_emit_integer_cast(Bytecode_Builder *builder, Type *target_type, Bytecode_Register operand_register)
{
    auto op_type = operand_register.type;
    assert(op_type != target_type);
    assert(op_type->kind == Type_Kind::INTEGER);

    auto dest_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, target_type);

    if (target_type->kind == Type_Kind::INTEGER) {

        if (target_type->bit_size == op_type->bit_size) {
            Bytecode_Register result = {
                .kind = Bytecode_Register_Kind::TEMPORARY,
                .flags = operand_register.flags,
                .index = -1,
                .type = target_type,
            };

            return result;

        } else if (target_type->bit_size < op_type->bit_size) {

            bytecode_emit_instruction(builder, Bytecode_Opcode::TRUNC, operand_register, {}, dest_register);
            return dest_register;

        } else {
            // target bit size > op bit size
            if (target_type->integer.sign) {
                bytecode_emit_instruction(builder, Bytecode_Opcode::SEXT, operand_register, {}, dest_register);
                return dest_register;
            } else {
                bytecode_emit_instruction(builder, Bytecode_Opcode::ZEXT, operand_register, {}, dest_register);
                return dest_register;
            }
        }

    } else {
        assert(false);
    }

    assert(false);
    return { .kind = Bytecode_Register_Kind::INVALID };
}

void bytecode_emit_print(Bytecode_Builder *builder, Bytecode_Register a)
{
    assert(a.kind == Bytecode_Register_Kind::TEMPORARY);

    bytecode_emit_instruction(builder, Bytecode_Opcode::PRINT, a, {}, {});
}

void bytecode_emit_push_arg(Bytecode_Builder *builder, Bytecode_Register arg_register)
{
    assert(arg_register.kind == Bytecode_Register_Kind::TEMPORARY);

    bytecode_emit_instruction(builder, Bytecode_Opcode::PUSH_ARG, arg_register, {}, {});
}

Bytecode_Register bytecode_emit_call(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle,
                        Bytecode_Register arg_count_register)
{
    assert(fn_handle >= 0 && fn_handle <= builder->functions.count);

    auto fn = &builder->functions[fn_handle];

    assert(arg_count_register.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(arg_count_register.flags & BC_REGISTER_FLAG_LITERAL);
    assert(arg_count_register.type->kind == Type_Kind::INTEGER);
    assert(arg_count_register.type->integer.sign);
    assert(arg_count_register.type->bit_size == 64);

    assert(fn->type->function.parameter_types.count == arg_count_register.value.integer.s64);

    Bytecode_Register fn_register = {
        .kind = Bytecode_Register_Kind::FUNCTION,
        .flags = BC_REGISTER_FLAG_LITERAL,
        .index = -1,
        .type = fn->type,
        .value = { .function_handle = fn_handle },
    };

    Bytecode_Register result_register = {};
    if (fn->type->function.return_type->kind != Type_Kind::VOID) {
        result_register = bytecode_register_create(builder,
                                                   Bytecode_Register_Kind::TEMPORARY,
                                                   fn->type->function.return_type);
    }

    auto op = Bytecode_Opcode::CALL;
    if (fn->flags & BC_FUNCTION_FLAG_FOREIGN) {
        op = Bytecode_Opcode::CALL_FOREIGN;
    }

    bytecode_emit_instruction(builder, op, fn_register, arg_count_register, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_call(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle,
                                     s64 arg_count)
{
    debug_assert(arg_count >= 0);

    auto arg_count_register = bytecode_integer_literal(builder, &builtin_type_s64, arg_count);
    return bytecode_emit_call(builder, fn_handle, arg_count_register);
}

Bytecode_Register bytecode_emit_call_pointer(Bytecode_Builder *builder, Bytecode_Register fn_ptr_reg, Bytecode_Register arg_count_register)
{
    assert(fn_ptr_reg.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(fn_ptr_reg.type->kind == Type_Kind::POINTER);
    assert(fn_ptr_reg.type->pointer.base->kind == Type_Kind::FUNCTION);

    assert(arg_count_register.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(arg_count_register.flags & BC_REGISTER_FLAG_LITERAL);
    assert(arg_count_register.type->kind == Type_Kind::INTEGER);
    assert(arg_count_register.type->integer.sign);
    assert(arg_count_register.type->bit_size == 64);

    auto fn_type = fn_ptr_reg.type->pointer.base;
    assert(arg_count_register.value.integer.s64 == fn_type->function.parameter_types.count);

    Bytecode_Register result_register = {};
    if (fn_type->function.return_type->kind != Type_Kind::VOID) {
        result_register = bytecode_register_create(builder,
                                                   Bytecode_Register_Kind::TEMPORARY,
                                                   fn_type->function.return_type);
    }


    bytecode_emit_instruction(builder, Bytecode_Opcode::CALL_PTR, fn_ptr_reg, arg_count_register, result_register);

    return result_register;

}

Bytecode_Register bytecode_emit_call_pointer(Bytecode_Builder *builder, Bytecode_Register fn_ptr_reg, s64 arg_count)
{
    assert(fn_ptr_reg.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(fn_ptr_reg.type->kind == Type_Kind::POINTER);
    assert(fn_ptr_reg.type->pointer.base->kind == Type_Kind::FUNCTION);

    assert(arg_count >= 0);
    auto arg_count_register = bytecode_integer_literal(builder, &builtin_type_s64, arg_count);

    return bytecode_emit_call_pointer(builder, fn_ptr_reg, arg_count_register);
}

void bytecode_emit_return(Bytecode_Builder *builder)
{
#ifndef NDEBUG
    auto fn = builder->functions[builder->insert_fn_index];
#endif
    assert(fn.type->function.return_type->kind == Type_Kind::VOID);

    bytecode_emit_instruction(builder, Bytecode_Opcode::RETURN_VOID, {}, {}, {});
}

void bytecode_emit_return(Bytecode_Builder *builder, Bytecode_Register return_value)
{
#ifndef NDEBUG
    auto fn = builder->functions[builder->insert_fn_index];
#endif
    assert(fn.type);
    assert(fn.type->function.return_type == return_value.type);

    bytecode_emit_instruction(builder, Bytecode_Opcode::RETURN, return_value, {}, {});
}

Bytecode_Register bytecode_emit_alloc(Bytecode_Builder *builder, Type *type, const char *name)
{
    Bytecode_Register type_register = bytecode_type_value(builder, type);
    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::ALLOC, type, BC_REGISTER_FLAG_NONE, name);
    bytecode_emit_instruction(builder, Bytecode_Opcode::ALLOC, type_register, {}, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_address_of(Bytecode_Builder *builder, Bytecode_Register alloc)
{
    if (!alloc.type->pointer_to) {
        get_pointer_type(alloc.type, &builder->zodiac_context->ast_allocator);
    }

    assert(alloc.type->pointer_to);

    Bytecode_Register result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, alloc.type->pointer_to);

    bytecode_emit_instruction(builder, Bytecode_Opcode::ADDROF, alloc, {}, result);

    return result;
}

Bytecode_Register bytecode_emit_address_of_function(Bytecode_Builder *builder, Bytecode_Function_Handle fn_handle)
{
    assert(fn_handle >= 0 && fn_handle < builder->functions.count);
    auto fn = &builder->functions[fn_handle];

    if (!fn->type->pointer_to) {
        get_pointer_type(fn->type, &builder->zodiac_context->ast_allocator);
    }

    assert(fn->type->pointer_to);

    Bytecode_Register result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, fn->type->pointer_to);

    Bytecode_Register fn_register = {
        .kind = Bytecode_Register_Kind::FUNCTION,
        .flags = BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT,
        .index = -1,
        .type = fn->type,
        .value = { .function_handle = fn_handle },
    };

    bytecode_emit_instruction(builder, Bytecode_Opcode::ADDROF_FUNC, fn_register, {}, result);

    return result;
}

void bytecode_emit_store_global(Bytecode_Builder *builder, Bytecode_Register source, Bytecode_Global_Handle global_handle)
{
    assert(source.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(global_handle >= 0 && global_handle < builder->globals.count);

    Bytecode_Global global = builder->globals[global_handle];
    assert(source.type == global.type);

    Bytecode_Register dest_reg = {
        .kind = Bytecode_Register_Kind::GLOBAL,
        .flags = BC_REGISTER_FLAG_NONE,
        .index = global_handle,
        .type = global.type,
    };

    bytecode_emit_instruction(builder, Bytecode_Opcode::STORE_G, source, dest_reg, {});
}

Bytecode_Register bytecode_emit_load_global(Bytecode_Builder *builder, Bytecode_Global_Handle global_handle)
{
    assert(global_handle >= 0);
    assert(global_handle < builder->globals.count);

    auto global = &builder->globals[global_handle];

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, global->type, BC_REGISTER_FLAG_NONE);

    Bytecode_Register source_reg = {
        .kind = Bytecode_Register_Kind::GLOBAL,
        .flags = BC_REGISTER_FLAG_NONE,
        .index = global_handle,
        .type = global->type,
    };

    bytecode_emit_instruction(builder, Bytecode_Opcode::LOAD_G, source_reg, {}, result_register);

    return result_register;
}

void bytecode_emit_store_alloc(Bytecode_Builder *builder, Bytecode_Register source, Bytecode_Register dest)
{
    assert(source.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(dest.kind == Bytecode_Register_Kind::ALLOC);
    assert(source.type == dest.type);

    bytecode_emit_instruction(builder, Bytecode_Opcode::STORE_A, source, dest, {});
}

Bytecode_Register bytecode_emit_load_alloc(Bytecode_Builder *builder, Bytecode_Register source)
{
    assert(source.kind == Bytecode_Register_Kind::ALLOC);

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, source.type, BC_REGISTER_FLAG_NONE);

    bytecode_emit_instruction(builder, Bytecode_Opcode::LOAD_A, source, {}, result_register);

    return result_register;
}

void bytecode_emit_store_pointer(Bytecode_Builder *builder, Bytecode_Register source, Bytecode_Register dest)
{
    assert(source.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(dest.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(dest.type->kind == Type_Kind::POINTER);
    assert(dest.type->pointer.base == source.type);

    bytecode_emit_instruction(builder, Bytecode_Opcode::STORE_PTR, source, dest, {});
}

Bytecode_Register bytecode_emit_load_pointer(Bytecode_Builder *builder, Bytecode_Register source)
{
    assert(source.type->kind == Type_Kind::POINTER);
    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, source.type->pointer.base, BC_REGISTER_FLAG_NONE);

    bytecode_emit_instruction(builder, Bytecode_Opcode::LOAD_PTR, source, {}, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_insert_value(Bytecode_Builder *builder, Bytecode_Register aggregate, Bytecode_Register new_elem_val, Type *struct_type, s64 index)
{
    assert(struct_type);
    assert(struct_type->flags & TYPE_FLAG_AGGREGATE);
    assert(struct_type->kind == Type_Kind::STRUCTURE);

    if (aggregate.kind == Bytecode_Register_Kind::INVALID) {
        assert(aggregate.type == nullptr);
        aggregate.kind = Bytecode_Register_Kind::UNDEF;
        aggregate.type = struct_type;
    }

    auto members = struct_type->structure.member_types;
    assert(members.count > index);

#ifndef NDEBUG
    Type *mem_type = members[index];
    assert(mem_type == new_elem_val.type);
#endif

    Bytecode_Register result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, struct_type, BC_REGISTER_FLAG_NONE);

    auto handle = bytecode_emit_instruction(builder, Bytecode_Opcode::INSERT_VALUE, aggregate, new_elem_val, result);
    auto ip = bytecode_get_instruction(builder, handle);
    ip->additional_index = index;

    return result;
}

Bytecode_Register bytecode_emit_extract_value(Bytecode_Builder *builder, Bytecode_Register aggregate, s64 index)
{
    // @TODO: @CLEANUP: This should be checked in a validation pass
    assert_msg(aggregate.kind == Bytecode_Register_Kind::TEMPORARY,
               "[Bytecode] aggregate register passed to bytecode_emit_extract_value must be a temporary register");

    auto agg_type = aggregate.type;
    assert(agg_type->flags & TYPE_FLAG_AGGREGATE);
    assert(agg_type->kind == Type_Kind::STRUCTURE);

    auto member_types = agg_type->structure.member_types;
    assert(index < member_types.count);

    auto member_type = member_types[index];

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, member_type, BC_REGISTER_FLAG_NONE);
    Bytecode_Register index_value = bytecode_integer_literal(builder, &builtin_type_s32, index);

    bytecode_emit_instruction(builder, Bytecode_Opcode::EXTRACT_VALUE, aggregate, index_value, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_insert_element(Bytecode_Builder *builder, Bytecode_Register array, Bytecode_Register new_elem_val, Type *array_type, s64 index)
{
    assert(array_type->kind == Type_Kind::STATIC_ARRAY);

    if (array.kind == Bytecode_Register_Kind::INVALID) {
        assert(array.type == nullptr);
        array.kind = Bytecode_Register_Kind::UNDEF;
        array.type = array_type;
    }

    assert(new_elem_val.type == array_type->static_array.element_type);
    assert(index >= 0 && index < array_type->static_array.count);

    Bytecode_Register result = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, array_type);

    auto handle = bytecode_emit_instruction(builder, Bytecode_Opcode::INSERT_ELEMENT, array, new_elem_val, result);
    auto ip = bytecode_get_instruction(builder, handle);
    ip->additional_index = index;

    return result;
}


Bytecode_Register bytecode_emit_extract_element(Bytecode_Builder *builder, Bytecode_Register array, s64 index)
{
    // @TODO: @CLEANUP: This should be checked in a validation pass
    assert_msg(array.kind == Bytecode_Register_Kind::TEMPORARY, "[Bytecode] array register passed to bytecode_emit_extract_element must be a temporary register");

    assert(array.type);

    Type *array_type = array.type;
    assert(array_type->kind == Type_Kind::STATIC_ARRAY);

    assert(index >= 0 && index < array_type->static_array.count);

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, array_type->static_array.element_type, BC_REGISTER_FLAG_NONE);
    Bytecode_Register index_value = bytecode_integer_literal(builder, &builtin_type_s64, index);

    bytecode_emit_instruction(builder, Bytecode_Opcode::EXTRACT_ELEMENT, array, index_value, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_aggregate_offset_pointer(Bytecode_Builder *builder, Bytecode_Register agg_register, s64 index)
{
    Type *agg_type = nullptr;

    if (agg_register.kind == Bytecode_Register_Kind::ALLOC) {
        agg_type = agg_register.type;

    } else if (agg_register.kind == Bytecode_Register_Kind::TEMPORARY) {
        assert(agg_register.type->kind == Type_Kind::POINTER);
        agg_type = agg_register.type->pointer.base;
    } else {
        assert_msg(agg_type, "Aggregate register is not a TEMPORARY or ALLOC.");
    }

    assert(agg_type->flags & TYPE_FLAG_AGGREGATE);
    assert(agg_type->kind == Type_Kind::STRUCTURE);

    auto &member_types = agg_type->structure.member_types;

    assert(index >= 0);
    assert(index < member_types.count);

    auto index_register = bytecode_integer_literal(builder, &builtin_type_s32, index);

    Type *member_type = member_types[index];
    Type *result_type = get_pointer_type(member_type, &builder->zodiac_context->ast_allocator);

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, result_type, BC_REGISTER_FLAG_NONE);
    bytecode_emit_instruction(builder, Bytecode_Opcode::AGG_OFFSET_POINTER, agg_register, index_register, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_array_offset_pointer(Bytecode_Builder *builder, Bytecode_Register array_register, s64 index)
{
    auto index_register = bytecode_integer_literal(builder, &builtin_type_s64, index);
    return bytecode_emit_array_offset_pointer(builder, array_register, index_register);
}

Bytecode_Register bytecode_emit_array_offset_pointer(Bytecode_Builder *builder, Bytecode_Register array_register, Bytecode_Register index_register)
{
    Type *array_type = nullptr;

    if (array_register.kind == Bytecode_Register_Kind::ALLOC ||
        array_register.kind == Bytecode_Register_Kind::GLOBAL) {

        array_type = array_register.type;

    } else {
        assert(array_register.type->kind == Type_Kind::POINTER);
        array_type = array_register.type->pointer.base;
    }

    assert(array_type);
    assert(array_type->kind == Type_Kind::STATIC_ARRAY);

    if (index_register.flags & BC_REGISTER_FLAG_LITERAL) {
        assert(index_register.flags & BC_REGISTER_FLAG_CONSTANT)
        assert(index_register.value.integer.s64 >= 0);
        assert(index_register.value.integer.s64 < array_type->static_array.count);
    }

    auto element_type = array_type->static_array.element_type;
    Type *result_type = get_pointer_type(element_type, &builder->zodiac_context->ast_allocator);

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, result_type, BC_REGISTER_FLAG_NONE);
    bytecode_emit_instruction(builder, Bytecode_Opcode::ARR_OFFSET_POINTER, array_register, index_register, result_register);

    return result_register;
}

Bytecode_Register bytecode_emit_ptr_offset_pointer(Bytecode_Builder *builder, Bytecode_Register ptr_reg, s64 index)
{
    auto index_register = bytecode_integer_literal(builder, &builtin_type_s64, index);
    return bytecode_emit_ptr_offset_pointer(builder, ptr_reg, index_register);
}

Bytecode_Register bytecode_emit_ptr_offset_pointer(Bytecode_Builder *builder, Bytecode_Register ptr_reg, Bytecode_Register index_register)
{
    auto ptr_type = ptr_reg.type;
    assert(ptr_type->kind == Type_Kind::POINTER);

    Bytecode_Register result_register = bytecode_register_create(builder, Bytecode_Register_Kind::TEMPORARY, ptr_type, BC_REGISTER_FLAG_NONE);
    bytecode_emit_instruction(builder, Bytecode_Opcode::PTR_OFFSET_POINTER, ptr_reg, index_register, result_register);

    return result_register;
}

void bytecode_emit_jmp(Bytecode_Builder *builder, Bytecode_Block_Handle block)
{
    auto block_value = bytecode_block_value(builder, block);
    bytecode_emit_instruction(builder, Bytecode_Opcode::JMP, block_value, {}, {});
}

void bytecode_emit_jmp_if(Bytecode_Builder *builder, Bytecode_Register cond, Bytecode_Block_Handle then_block, Bytecode_Block_Handle else_block)
{
    auto then_block_value = bytecode_block_value(builder, then_block);
    auto else_block_value = bytecode_block_value(builder, else_block);

    bytecode_emit_instruction(builder, Bytecode_Opcode::JMP_IF, cond, then_block_value, else_block_value);
}

Bytecode_Instruction_Handle bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Opcode op, Bytecode_Register a, Bytecode_Register b, Bytecode_Register result)
{
    debug_assert(builder);
    assert(builder->insert_fn_index >= 0 &&
           builder->insert_fn_index < builder->functions.count);
    auto fn = &builder->functions[builder->insert_fn_index];
    assert(builder->insert_block_index >= 0 &&
           builder->insert_block_index < fn->blocks.count);

    if ((result.kind != Bytecode_Register_Kind::INVALID &&
         result.kind != Bytecode_Register_Kind::BLOCK)
            &&
        ((result.type->flags & TYPE_FLAG_AGGREGATE) ||
         result.type->kind == Type_Kind::STATIC_ARRAY ||
         op == Bytecode_Opcode::ALLOC)) {

        // These should be all the instructions that require stack size for their result
        assert(op == Bytecode_Opcode::ALLOC ||
               op == Bytecode_Opcode::LOAD_A ||
               op == Bytecode_Opcode::LOAD_G ||
               op == Bytecode_Opcode::INSERT_VALUE ||
               op == Bytecode_Opcode::EXTRACT_VALUE ||
               op == Bytecode_Opcode::CALL ||
               op == Bytecode_Opcode::CALL_PTR ||
               op == Bytecode_Opcode::LOAD_PTR ||
               op == Bytecode_Opcode::INSERT_ELEMENT);

        // @cleanup: @todo: @fixme: alignment
        assert(result.type->bit_size % 8 == 0);
        fn->required_stack_size += result.type->bit_size / 8;
    }

    Bytecode_Instruction instruction = {
        .op = op,
        .a = a,
        .b = b,
        .dest = result,
        .additional_index = 0,
    };

    auto block = &fn->blocks[builder->insert_block_index];
    auto instructions = &block->instructions;

    Bytecode_Instruction_Handle handle = {
        .fn_index = builder->insert_fn_index,
        .block_index = builder->insert_block_index,
        .instruction_index = (s64)instructions->count,
    };

    dynamic_array_append(instructions, instruction);

    if (bytecode_instruction_is_terminator(&instruction)) {
        block->terminated = true;
    }

    return handle;
}

Bytecode_Instruction *bytecode_get_instruction(Bytecode_Builder *bb, const Bytecode_Instruction_Handle &handle)
{
    assert(bb->functions.count > handle.fn_index);

    auto fn = &bb->functions[handle.fn_index];

    assert(fn->blocks.count > handle.block_index);

    auto block = &fn->blocks[handle.block_index];

    assert(block->instructions.count > handle.instruction_index);

    return &block->instructions[handle.instruction_index];
}

Atom bytecode_unique_global_name(Bytecode_Builder *bb, Atom name)
{
    debug_assert(bb);

    auto ta = temp_allocator();
    auto taa = temp_allocator_allocator();
    auto mark = temporary_allocator_get_mark(ta);

    auto original_name = name;
    int dup_num = 1;

    do {
        bool duplicate = false;
        for (s64 i = 0; i < bb->globals.count; i++) {
            auto glob = &bb->globals[i];
            if (glob->atom == name) {
                duplicate = true;
                break;
            }
        }

        if (!duplicate) {
            temporary_allocator_reset(ta, mark);
            return name;
        }

        name = atom_get(&bb->zodiac_context->atoms, string_format(taa, "%s%i", original_name.data, dup_num));
        dup_num += 1;

    } while (true);
}

String bytecode_unique_register_name_in_function(Bytecode_Builder *bb, Bytecode_Function_Handle fn_handle, String_Ref name)
{
    debug_assert(bb);

    assert(fn_handle >= 0 && fn_handle < bb->functions.count);

    auto ta = temp_allocator();
    auto taa = temp_allocator_allocator();
    auto mark = temporary_allocator_get_mark(ta);

    auto func = &bb->functions[fn_handle];

    auto original_name = name;
    int dup_num = 1;

    do {
        bool duplicate = false;
        for (s64 i = 0; i < func->registers.count; i++) {
            auto reg = &func->registers[i];
            if (string_equal(reg->alloc_name, name)) {
                duplicate = true;
                break;
            }
        }

        if (!duplicate) {
            temporary_allocator_reset(ta, mark);
            return string_copy(bb->bytecode_allocator, name);
        }

        name = string_format(taa, "%s%i", original_name.data, dup_num);
        dup_num += 1;

    } while(true);
}

Bytecode_Register bytecode_retype_literal(Type *target_type, Bytecode_Register operand_register)
{
    debug_assert(operand_register.flags & BC_REGISTER_FLAG_LITERAL);

    switch (target_type->kind) {
        default: assert(false && !"Unsupported type for bytecode_retype_literal"); break;

        case Type_Kind::INTEGER: {
            assert(operand_register.type->kind == Type_Kind::INTEGER);

            Bytecode_Register result = {
                .kind = Bytecode_Register_Kind::TEMPORARY,
                .flags = operand_register.flags,
                .index = -1,
                .type = target_type,
                .value = operand_register.value,
            };

            return result;

            break;
        }
    }

    assert(false);
    return {};
}

} }
