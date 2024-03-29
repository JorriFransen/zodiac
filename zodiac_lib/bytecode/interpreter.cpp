#include "bytecode/interpreter.h"

#include "atom.h"
#include "common.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "platform/platform.h"
#include "type.h"
#include "util/asserts.h"
#include "zodiac_context.h"

#include <string.h>

namespace Zodiac { namespace Bytecode {

Interpreter interpreter_create(Allocator *allocator, Zodiac_Context *context)
{
    Interpreter result;
    interpreter_init(allocator, context, &result);
    return result;
}

void interpreter_init(Allocator *allocator, Zodiac_Context *context, Interpreter *out_interp)
{
    debug_assert(allocator && context && out_interp);

    out_interp->allocator = allocator;
    out_interp->context = context;
    out_interp->functions = {};

    out_interp->running = false;
    out_interp->start_return_value = {};
    out_interp->jumped_from_block = -1;

    stack_init(allocator, &out_interp->frames);
    stack_init(allocator, &out_interp->arg_stack);

    dynamic_array_create(allocator, &out_interp->globals);

    const auto register_count = 128;
    out_interp->registers.data = alloc_array<Interpreter_Register>(allocator, register_count);
    out_interp->registers.count = register_count;
    out_interp->used_register_count = 0;

    const auto stack_mem_size = KIBIBYTE(1);
    out_interp->first_stack_block.data = alloc_array<u8>(allocator, stack_mem_size);
    out_interp->first_stack_block.used = 0;
    out_interp->first_stack_block.cap = stack_mem_size;
    out_interp->first_stack_block.previous = nullptr;

    out_interp->current_stack_block = &out_interp->first_stack_block;
    out_interp->free_stack_blocks = nullptr;

    filesystem_stdout_file(&out_interp->std_out);

    out_interp->ffi = ffi_create(allocator, context, true, interpreter_handle_ffi_callback);
}

void interpreter_free(Interpreter *interp)
{
    stack_free(&interp->frames);
    stack_free(&interp->arg_stack);

    free(interp->allocator, interp->registers.data);

    free(interp->allocator, interp->first_stack_block.data);

    ffi_free(&interp->ffi);
}

Interpreter_Register interpreter_start(Interpreter *interp, Bytecode_Program program)
{
    return interpreter_start(interp, program.functions, program.foreign_functions, program.globals, program.required_global_size, program.entry_handle);
}

Interpreter_Register interpreter_start(Interpreter *interp, Bytecode_Program program, Bytecode_Function_Handle entry_handle)
{
    return interpreter_start(interp, program.functions, program.foreign_functions, program.globals, program.required_global_size, entry_handle);
}

Interpreter_Register interpreter_start(Interpreter *interp, Array_Ref<Bytecode_Function> functions, Array_Ref<Bytecode_Function_Handle> foreign_functions, Array_Ref<Bytecode_Global> globals, s64 globals_size, Bytecode_Function_Handle fn_handle)
{
    assert(interp);
    assert(functions.count);

    interp->start_return_value = {};
    interp->jumped_from_block = -1;
    interp->globals.count = 0;
    if (interp->global_mem) free(interp->allocator, interp->global_mem);

    assert(interp->first_stack_block.used == 0);
    assert(interp->current_stack_block == &interp->first_stack_block);

    // Setup the _type_info_table
    if (interp->context->type_infos.count) {
        auto type_info_pointer_type = get_pointer_type(get_type_info_type(interp->context), &interp->context->ast_allocator);

        auto bb = interp->context->bytecode_builder;

        Bytecode_Register arr_ptr_val = bytecode_pointer_literal(bb, type_info_pointer_type->pointer_to, interp->context->type_infos.data);

        bool found = false;
        for (s64 i = 0; i < bb->globals.count; i++) {
            auto global = &bb->globals[i];


            if (global->atom == "_type_info_pointers") {

                assert(global->type->kind == Type_Kind::SLICE);
                Bytecode_Register members[2] = {
                    arr_ptr_val,
                    bytecode_integer_literal(bb, &builtin_type_s64, interp->context->type_infos.count)
                };
                global->initial_value = bytecode_aggregate_literal(bb, members, global->type->slice.struct_type);
                global->initial_value.type = global->type;
                found = true;
                break;
            }
        }
        assert(found);
    }


    interp->functions = functions;

    for (s64 i = 0; i < foreign_functions.count; i++) {

        auto ffn_handle = foreign_functions[i];
        assert(ffn_handle >= 0 && ffn_handle < interp->functions.count);
        auto ffn = &interp->functions[ffn_handle];
        assert(ffn->flags & BC_FUNCTION_FLAG_FOREIGN);

        FFI_Handle ffi_handle = ffi_load_function(&interp->ffi, ffn->name);
        assert(ffi_handle);

        assert(ffn->ffi_handle == nullptr || ffn->ffi_handle == ffi_handle);
        ffn->ffi_handle = ffi_handle;
    }

    if (globals.count) {
        assert(globals_size);
        interp->global_mem = alloc_array<u8>(interp->allocator, globals_size);
        auto glob_cur = interp->global_mem;

#ifndef NDEBUG
        auto glob_end = interp->global_mem + globals_size;
#endif

        for (s64 i = 0; i < globals.count; i++) {

            auto global = globals[i];

            Interpreter_Register glob_reg = {
                .type = global.type,
                .pointer = glob_cur,
            };

            // @Cleanup: @TODO: @FIXME: alignment
            assert(global.type->bit_size % 8 == 0);
            auto size = global.type->bit_size / 8;
            glob_cur += size;

            if (globals[i].initial_value.kind != Bytecode_Register_Kind::INVALID) {

                assert(global.initial_value.kind == Bytecode_Register_Kind::TEMPORARY ||
                       global.initial_value.kind == Bytecode_Register_Kind::FUNCTION ||
                       global.initial_value.kind == Bytecode_Register_Kind::ZEROINITIALIZER);

                Interpreter_Register initial_value = interpreter_load_register(interp, globals[i].initial_value);
                interpreter_store_pointer(interp, initial_value, glob_reg.pointer);

            } else {
                memset(glob_reg.pointer, 0, size);
            }

            dynamic_array_append(&interp->globals, glob_reg);
        }
        assert(glob_cur == glob_end);
    }

    assert(fn_handle >= 0 && fn_handle < functions.count);
    auto entry_fn = &interp->functions[fn_handle];

    interpreter_push_stack_frame(interp, fn_handle, 0, -1);

    interp->start_return_value = {
        .type = entry_fn->type->function.return_type,
    };

    interp->running = true;
    while (interp->running) {
        auto instruction = interpreter_fetch_instruction(interp);
        interpreter_execute_instruction(interp, instruction);
    }

    return interp->start_return_value;
}

Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp)
{
    auto frame = stack_top_ptr(&interp->frames);

    assert(frame->fn_handle >= 0 && frame->fn_handle < interp->functions.count);
    auto fn = &interp->functions[frame->fn_handle];
    assert(frame->bp >= 0 && frame->bp < fn->blocks.count);
    Bytecode_Block *block = &fn->blocks[frame->bp];
    assert(frame->ip >= 0 && frame->ip < block->instructions.count);

    return block->instructions[frame->ip];
}

void interpreter_execute_instruction(Interpreter *interp, Bytecode_Instruction instruction)
{

    bool advance_ip = true;
    auto frame = stack_top_ptr(&interp->frames);

    assert(frame->fn_handle >= 0 && frame->fn_handle < interp->functions.count);
    auto current_function = &interp->functions[frame->fn_handle];
    assert(frame->bp >= 0 && frame->bp < current_function->blocks.count);

#ifndef NDEBUG
    Bytecode_Block *block = &current_function->blocks[frame->bp];
    assert(frame->ip >= 0 && frame->ip < block->instructions.count);
#endif

    switch (instruction.op) {
        case Bytecode_Opcode::NOP: assert(false); break;

#define INTEGER_BINOP_CASE(op) { \
Interpreter_Register lhs = interpreter_load_register(interp, instruction.a); \
Interpreter_Register rhs = interpreter_load_register(interp, instruction.b); \
assert(lhs.type == rhs.type); \
assert(lhs.type->kind == Type_Kind::INTEGER); \
Bytecode_Register_Value rv = {}; \
auto lhsi = lhs.value.integer; \
auto rhsi = rhs.value.integer; \
if (lhs.type->integer.sign) { \
    switch (lhs.type->bit_size) { \
        default: assert(false); break; \
        case 8:   rv.integer.s8  = lhsi.s8  op rhsi.s8; break; \
        case 16:  rv.integer.s16 = lhsi.s16 op rhsi.s16; break; \
        case 32:  rv.integer.s32 = lhsi.s32 op rhsi.s32; break; \
        case 64:  rv.integer.s64 = lhsi.s64 op rhsi.s64; break; \
    } \
} else { \
    switch (lhs.type->bit_size) { \
        default: assert(false); break; \
        case 8:   rv.integer.u8  = lhsi.u8  op rhsi.u8; break; \
        case 16:  rv.integer.u16 = lhsi.u16 op rhsi.u16; break; \
        case 32:  rv.integer.u32 = lhsi.u32 op rhsi.u32; break; \
        case 64:  rv.integer.u64 = lhsi.u64 op rhsi.u64; break; \
    } \
} \
Interpreter_Register result_register = { \
    .type = lhs.type, \
    .value = rv, \
}; \
interpreter_store_register(interp, result_register, instruction.dest); \
break; \
}

        case Bytecode_Opcode::I_ADD: INTEGER_BINOP_CASE(+)
        case Bytecode_Opcode::I_SUB: INTEGER_BINOP_CASE(-)
        case Bytecode_Opcode::I_MUL: INTEGER_BINOP_CASE(*)
        case Bytecode_Opcode::I_DIV: INTEGER_BINOP_CASE(/)
        case Bytecode_Opcode::I_MOD: INTEGER_BINOP_CASE(%)

#define INTEGER_BINOP_CMP_CASE(op) { \
Interpreter_Register lhs = interpreter_load_register(interp, instruction.a); \
Interpreter_Register rhs = interpreter_load_register(interp, instruction.b); \
assert(lhs.type == rhs.type); \
assert(lhs.type->kind == Type_Kind::INTEGER || lhs.type->kind == Type_Kind ::BOOLEAN || lhs.type->kind == Type_Kind::ENUM); \
Bytecode_Register_Value rv = {}; \
auto lhsi = lhs.value.integer; \
auto rhsi = rhs.value.integer; \
if (lhs.type->integer.sign) { \
    switch (lhs.type->bit_size) { \
        default: assert(false); break; \
        case 8: rv.boolean = lhsi.s8 op rhsi.s8; break; \
        case 16: rv.boolean = lhsi.s16 op rhsi.s16; break; \
        case 32: rv.boolean = lhsi.s32 op rhsi.s32; break; \
        case 64: rv.boolean = lhsi.s64 op rhsi.s64; break; \
    } \
} else { \
    switch (lhs.type->bit_size) { \
        default: assert(false); break; \
        case 8: rv.boolean = lhsi.u8 op rhsi.u8; break; \
        case 16: rv.boolean = lhsi.u16 op rhsi.u16; break; \
        case 32: rv.boolean = lhsi.u32 op rhsi.u32; break; \
        case 64: rv.boolean = lhsi.u64 op rhsi.u64; break; \
    } \
} \
Interpreter_Register result_register = { \
    .type = &builtin_type_bool, \
    .value = rv, \
}; \
interpreter_store_register(interp, result_register, instruction.dest); \
break; \
}

        case Bytecode_Opcode::I_EQ: INTEGER_BINOP_CMP_CASE(==)
        case Bytecode_Opcode::I_NEQ: INTEGER_BINOP_CMP_CASE(!=)
        case Bytecode_Opcode::I_GT: INTEGER_BINOP_CMP_CASE(>)
        case Bytecode_Opcode::I_LT: INTEGER_BINOP_CMP_CASE(<)
        case Bytecode_Opcode::I_GT_EQ: INTEGER_BINOP_CMP_CASE(>=)
        case Bytecode_Opcode::I_LT_EQ: INTEGER_BINOP_CMP_CASE(<=)

#undef INTEGER_BINOP_CMP_CASE

#define FLOAT_BINOP_CASE(op) { \
Interpreter_Register lhs = interpreter_load_register(interp, instruction.a); \
Interpreter_Register rhs = interpreter_load_register(interp, instruction.b); \
assert(lhs.type == rhs.type); \
assert(lhs.type->kind == Type_Kind::FLOAT); \
Bytecode_Register_Value rv = {}; \
auto lhsr = lhs.value.real; \
auto rhsr = rhs.value.real; \
switch (lhs.type->bit_size) { \
    default: assert(false && "FLOAT_BINOP_CASE unsupported bit width"); break; \
    case 32:  rv.real.r32 = lhsr.r32 op rhsr.r32; break; \
    case 64:  rv.real.r64 = lhsr.r64 op rhsr.r64; break; \
} \
Interpreter_Register result_register = { \
    .type = lhs.type, \
    .value = rv, \
}; \
interpreter_store_register(interp, result_register, instruction.dest); \
break; \
}

        case Bytecode_Opcode::F_ADD: FLOAT_BINOP_CASE(+)
        case Bytecode_Opcode::F_SUB: FLOAT_BINOP_CASE(-)
        case Bytecode_Opcode::F_MUL: FLOAT_BINOP_CASE(*)
        case Bytecode_Opcode::F_DIV: FLOAT_BINOP_CASE(/)

#undef FLOAT_BINOP_CASE

#define FLOAT_BINOP_CMP_CASE(op) { \
Interpreter_Register lhs = interpreter_load_register(interp, instruction.a); \
Interpreter_Register rhs = interpreter_load_register(interp, instruction.b); \
assert(lhs.type == rhs.type); \
assert(lhs.type->kind == Type_Kind::FLOAT); \
Bytecode_Register_Value rv = {}; \
auto lhsr = lhs.value.real; \
auto rhsr = rhs.value.real; \
switch (lhs.type->bit_size) { \
    default: assert(false && !"FLOAT_BINOP_CMP_CASE Unhandled float bit width"); break; \
    case 32: rv.boolean = lhsr.r32 op rhsr.r32; break; \
    case 64: rv.boolean = lhsr.r64 op rhsr.r64; break; \
} \
Interpreter_Register result_register = { \
    .type = &builtin_type_bool, \
    .value = rv, \
}; \
interpreter_store_register(interp, result_register, instruction.dest); \
break; \
}

        case Bytecode_Opcode::F_EQ: FLOAT_BINOP_CMP_CASE(==)
        case Bytecode_Opcode::F_NEQ: FLOAT_BINOP_CMP_CASE(!=)
        case Bytecode_Opcode::F_GT: FLOAT_BINOP_CMP_CASE(>)
        case Bytecode_Opcode::F_LT: FLOAT_BINOP_CMP_CASE(<)
        case Bytecode_Opcode::F_GT_EQ: FLOAT_BINOP_CMP_CASE(>=)
        case Bytecode_Opcode::F_LT_EQ: FLOAT_BINOP_CMP_CASE(<=)

#undef FLOAT_BINOP_CMP_CASE

#define PTR_BINOP_CMP_CASE(op) \
        { \
            Interpreter_Register lhs = interpreter_load_register(interp, instruction.a); \
            Interpreter_Register rhs = interpreter_load_register(interp, instruction.b); \
            assert(lhs.type == rhs.type); \
            assert(lhs.type->kind == Type_Kind::POINTER); \
            Interpreter_Register result_register = { \
                .type = &builtin_type_bool, \
            }; \
            result_register.value.boolean = lhs.pointer op rhs.pointer; \
            interpreter_store_register(interp, result_register, instruction.dest); \
            break; \
        }

        case Bytecode_Opcode::PTR_EQ: PTR_BINOP_CMP_CASE(==)
        case Bytecode_Opcode::PTR_NEQ: PTR_BINOP_CMP_CASE(!=)

#undef PTR_BINOP_CMP_CASE

        case Bytecode_Opcode::XOR: {
            Interpreter_Register lhs = interpreter_load_register(interp, instruction.a);
            Interpreter_Register rhs = interpreter_load_register(interp, instruction.b);

            assert(lhs.type == rhs.type);
            assert(lhs.type == &builtin_type_bool);

            Bytecode_Register_Value rv = { .boolean = (bool)(lhs.value.boolean ^ rhs.value.boolean) };

            Interpreter_Register result_register = {
                .type = lhs.type,
                .value = rv,
            };

            interpreter_store_register(interp, result_register, instruction.dest);

            break;
        }

        case Bytecode_Opcode::SQRT: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);

            Bytecode_Register_Value rv = {};

            if (operand.type->kind == Type_Kind::INTEGER) {

                if (operand.type->integer.sign) {
                    switch (operand.type->bit_size) {
                        default: assert(false && !"SQRT unhandled int bit width"); break;
                        case 8: rv.integer.s8 = (u8)platform_sqrt(operand.value.integer.s8); break;
                        case 16: rv.integer.s16 = (u16)platform_sqrt(operand.value.integer.s16); break;
                        case 32: rv.integer.s32 = (u32)platform_sqrt(operand.value.integer.s32); break;
                        case 64: rv.integer.s64 = (u64)platform_sqrt(operand.value.integer.s64); break;
                    }
                }

            } else if (operand.type->kind == Type_Kind::FLOAT) {

                switch (operand.type->bit_size) {
                    default: assert(false && !"SQRT unhandled float bit width"); break;
                    case 32: rv.real.r32 = platform_sqrt(operand.value.real.r32); break;
                    case 64: rv.real.r64 = platform_sqrt(operand.value.real.r64); break;
                }

            } else {
                assert(false);
            }

            Interpreter_Register result_register = {
                .type = operand.type,
                .value = rv,
            };

            interpreter_store_register(interp, result_register, instruction.dest);
            break;
        }

        case Bytecode_Opcode::TRUNC: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);
            Interpreter_Register result = {
                .type = instruction.dest.type,
            };

            assert(instruction.dest.type->kind == Type_Kind::INTEGER);
            assert(instruction.dest.type->bit_size < operand.type->bit_size);

            Bytecode_Register_Value rv = {};

#define TRUNC_CASE_(size) case size: { \
switch (operand.type->bit_size) { \
    default: assert(false); break; \
    case 8: rv.integer.u##size = (u##size)operand.value.integer.u8; break; \
    case 16: rv.integer.u##size = (u##size)operand.value.integer.u16; break; \
    case 32: rv.integer.u##size = (u##size)operand.value.integer.u32; break; \
    case 64: rv.integer.u##size = (u##size)operand.value.integer.u64; break; \
} break; }

            switch (instruction.dest.type->bit_size) {
                default: assert(false); break;
                TRUNC_CASE_(8)
                TRUNC_CASE_(16)
                TRUNC_CASE_(32)
                TRUNC_CASE_(64)
            }

#undef TRUNC_CASE_

            result.value = rv;

            interpreter_store_register(interp, result, instruction.dest);

            break;
        }

        case Bytecode_Opcode::SEXT: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);
            Interpreter_Register result = {
                .type = instruction.dest.type,
            };

            assert(operand.type->kind == Type_Kind::INTEGER);
            assert(instruction.dest.type->kind == Type_Kind::INTEGER);
            assert(instruction.dest.type->bit_size > operand.type->bit_size);

            assert(operand.type->integer.sign);
            assert(instruction.dest.type->integer.sign);

            Bytecode_Register_Value rv = {};

#define SEXT_CASE_(size) case size: { \
switch (operand.type->bit_size) { \
    default: assert(false); break; \
    case 8: rv.integer.s##size = (u##size)operand.value.integer.s8; break; \
    case 16: rv.integer.s##size = (u##size)operand.value.integer.s16; break; \
    case 32: rv.integer.s##size = (u##size)operand.value.integer.s32; break; \
    case 64: rv.integer.s##size = (u##size)operand.value.integer.s64; break; \
} break; }

            switch (instruction.dest.type->bit_size) {
                default: assert(false); break;
                SEXT_CASE_(8)
                SEXT_CASE_(16)
                SEXT_CASE_(32)
                SEXT_CASE_(64)
            }

#undef SEXT_CASE_

            result.value = rv;

            interpreter_store_register(interp, result, instruction.dest);

            break;
        }

        case Bytecode_Opcode::ZEXT: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);
            Interpreter_Register result = {
                .type = instruction.dest.type,
            };

            assert(operand.type->kind == Type_Kind::INTEGER);
            assert(instruction.dest.type->kind == Type_Kind::INTEGER);
            assert(instruction.dest.type->bit_size > operand.type->bit_size);

            assert(!operand.type->integer.sign);
            assert(!instruction.dest.type->integer.sign);

            Bytecode_Register_Value rv = {};

            #define ZEXT_CASE_(size) case size: { \
                switch (operand.type->bit_size) { \
                    default: assert(false); break; \
                    case 8: rv.integer.u##size = (u##size)operand.value.integer.u8; break; \
                    case 16: rv.integer.u##size = (u##size)operand.value.integer.u16; break; \
                    case 32: rv.integer.u##size = (u##size)operand.value.integer.u32; break; \
                    case 64: rv.integer.u##size = (u##size)operand.value.integer.u64; break; \
            } break; }

            switch (instruction.dest.type->bit_size) {
                default: assert(false); break;
                ZEXT_CASE_(8)
                ZEXT_CASE_(16)
                ZEXT_CASE_(32)
                ZEXT_CASE_(64)
            }

            #undef ZEXT_CASE_

            result.value = rv;

            interpreter_store_register(interp, result, instruction.dest);

            break;
        }

        case Bytecode_Opcode::BITCAST: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);
            Interpreter_Register result = {
                .type = instruction.dest.type,
            };

            assert(operand.type->bit_size == result.type->bit_size);
            assert(operand.type->bit_size / 8 <= sizeof(result.value));

            auto source_ptr = &operand.value;
            auto dest_ptr = &result.value;

            zmemcpy(dest_ptr, source_ptr, operand.type->bit_size / 8);

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }

        case Bytecode_Opcode::FCAST: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);
            Interpreter_Register result = {
                .type = instruction.dest.type,
            };

            auto source_ptr = &operand.value;
            auto dest_ptr = &result.value;

            if (operand.type->bit_size == 32) {
                assert(result.type->bit_size == 64);
                dest_ptr->real.r64 = source_ptr->real.r32;
            } else {
                assert(operand.type->bit_size == 64);
                assert(result.type->bit_size == 32);
                dest_ptr->real.r32 = source_ptr->real.r64;
            }

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }

        case Bytecode_Opcode::BOOL_TO_INT: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);

            Interpreter_Register result = {
                .type = instruction.dest.type,
            };

            result.value.integer.u64 = operand.value.boolean;

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }

        case Bytecode_Opcode::PUSH_ARG: {
            Interpreter_Register arg = interpreter_load_register(interp, instruction.a);
            stack_push(&interp->arg_stack, arg);
            break;
        }

        case Bytecode_Opcode::CALL: {
            assert(instruction.a.kind == Bytecode_Register_Kind::FUNCTION);
            Interpreter_Register arg_count = interpreter_load_register(interp, instruction.b);
            assert(arg_count.type == &builtin_type_s64);
            assert(arg_count.value.integer.s64 <= stack_count(&interp->arg_stack));

            advance_ip = false;
            // We need to advance here since pushing a new stack frame here might cause the frame stack to grow.
            //  The frames are stored by value in the stack, but at the end of this function we advance the ip
            //  via a pointer to the frame. So when the push of the new frame causes the stack to grow (reallocate)
            //  this pointer will be invalid.
            frame->ip += 1;

            interpreter_push_stack_frame(interp, instruction.a.value.function_handle, arg_count.value.integer.s64, instruction.dest.index);
            break;
        }

        case Bytecode_Opcode::CALL_FOREIGN: {
            assert(instruction.a.kind == Bytecode_Register_Kind::FUNCTION);
            Interpreter_Register arg_count_reg = interpreter_load_register(interp, instruction.b);
            assert(arg_count_reg.type == &builtin_type_s64);
            assert(arg_count_reg.value.integer.s64 <= stack_count(&interp->arg_stack));

            auto arg_count = arg_count_reg.value.integer.s64;

            interpreter_call_foreign_function(interp, instruction.a.value.function_handle, arg_count, instruction.dest.index);
            break;
        }

        case Bytecode_Opcode::CALL_PTR: {
            assert(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);
            assert(instruction.a.type->kind == Type_Kind::FUNCTION);

            Interpreter_Register arg_count_reg = interpreter_load_register(interp, instruction.b);
            assert(arg_count_reg.type == &builtin_type_s64);
            assert(arg_count_reg.value.integer.s64 <= stack_count(&interp->arg_stack));

            auto arg_count = arg_count_reg.value.integer.s64;

            interpreter_call_pointer(interp, instruction.a, arg_count, instruction.dest.index);
            break;
        }

        case Bytecode_Opcode::RETURN_VOID: {
            interpreter_pop_stack_frame(interp);
            advance_ip = false;
            if (!stack_count(&interp->frames)) {
                interp->running = false;
            }
            break;
        }

        case Bytecode_Opcode::RETURN: {
            Interpreter_Register return_value = interpreter_load_register(interp, instruction.a);
            auto old_frame = interpreter_pop_stack_frame(interp);
            advance_ip = false;
            if (!stack_count(&interp->frames)) {
                interp->running = false;
            }

            if (old_frame.dest_index >= 0) {
                auto new_frame = stack_top_ptr(&interp->frames);
                Bytecode_Register dest_register = {};
                if (new_frame->fn_handle != -1) {
                    current_function = &interp->functions[new_frame->fn_handle];
                    dest_register = current_function->registers[old_frame.dest_index];
                } else {
                    // This should be a dummy frame
                    dest_register = {
                        .kind = Bytecode_Register_Kind::TEMPORARY,
                        .index = old_frame.dest_index,
                        .type = return_value.type,
                    };
                }

                if ((return_value.type->flags & TYPE_FLAG_AGGREGATE) ||
                     return_value.type->kind == Type_Kind::STATIC_ARRAY) {

                    if (return_value.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL) {

                        Interpreter_Register dest_reg = interpreter_load_register(interp, dest_register);
                        if (!dest_reg.pointer) {
                            assert(return_value.type->bit_size % 8 == 0);
                            auto size = return_value.type->bit_size / 8;

                            dest_reg.pointer = interpreter_stack_alloc(interp, size);
                        }

                        interpreter_copy_compound_literal_into_memory(interp, dest_reg.pointer, return_value);
                        return_value = dest_reg;

                    } else {
                        interpreter_load_pointer(interp, return_value.pointer, &return_value, return_value.type);
                    }
                }
                interpreter_store_register(interp, return_value, dest_register);

            } else {
                // We are returning from the function interpreter_start was called with
                assert(interp->start_return_value.type == return_value.type);
                interp->start_return_value.value = return_value.value;
            }
            break;
        }

        case Bytecode_Opcode::ALLOC: {
            assert(instruction.a.kind == Bytecode_Register_Kind::TYPE);
            assert(instruction.dest.kind == Bytecode_Register_Kind::ALLOC);

            // @Cleanup: @TODO: @FIXME: alignment?
            assert (instruction.a.type->bit_size % 8 == 0);
            auto size = instruction.a.type->bit_size / 8;

            u8 *ptr = interpreter_stack_alloc(interp, size);

            Interpreter_Register alloc_register = {
                .type = instruction.a.type,
                { .pointer = ptr },
            };

            interpreter_store_register(interp, alloc_register, instruction.dest);
            break;
        }

        case Bytecode_Opcode::ADDROF: {
            assert(instruction.a.kind == Bytecode_Register_Kind::ALLOC ||
                   instruction.a.kind == Bytecode_Register_Kind::GLOBAL);

            Interpreter_Register alloc_register = interpreter_load_register(interp, instruction.a);

            assert(alloc_register.pointer);

            Interpreter_Register result = {
                .type = instruction.dest.type,
                .value = { .pointer = alloc_register.pointer }
            };

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }

        case Bytecode_Opcode::ADDROF_FUNC: {
            assert(instruction.a.kind == Bytecode_Register_Kind::FUNCTION);
            Bytecode_Function_Handle fn_handle = instruction.a.value.function_handle;
            assert(fn_handle >= 0 && fn_handle < interp->functions.count);

            Bytecode_Function *fn = &interp->functions[fn_handle];

            Interpreter_Register result = {
                .type = instruction.dest.type,
                .value = { .pointer = nullptr },
            };

            if (!(fn->flags & BC_FUNCTION_FLAG_FOREIGN) &&
                !fn->ffi_handle) {

                FFI_Function_User_Data func_data = { .interp = interp, .handle = fn_handle };
                FFI_Handle ffi_handle = ffi_create_callback(&interp->ffi, func_data, fn->type);
                assert(ffi_handle);
                fn->ffi_handle = ffi_handle;
            }

            assert(fn->ffi_handle);
            result.value.pointer = (u8 *)fn->ffi_handle;
            assert(result.value.pointer);

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }

        case Bytecode_Opcode::STORE_G: {
            Interpreter_Register new_value = interpreter_load_register(interp, instruction.a);
            Interpreter_Register global_register = interpreter_load_register(interp, instruction.b);

            assert(new_value.type == global_register.type);
            assert(global_register.pointer);

            interpreter_store_pointer(interp, new_value, global_register.pointer);
            break;
        }

        case Bytecode_Opcode::LOAD_G: {
            Interpreter_Register global_register = interpreter_load_register(interp, instruction.a);
            assert(global_register.pointer);

            assert(instruction.dest.kind == Bytecode_Register_Kind::TEMPORARY);
            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);

            interpreter_load_pointer(interp, global_register.pointer, &dest_reg, global_register.type);

            interpreter_store_register(interp, dest_reg, instruction.dest);
            break;
        }

        case Bytecode_Opcode::STORE_A: {
            Interpreter_Register new_value = interpreter_load_register(interp, instruction.a);
            Interpreter_Register alloc_register = interpreter_load_register(interp, instruction.b);

            assert(alloc_register.pointer);

            interpreter_store_pointer(interp, new_value, alloc_register.pointer);
            break;
        }

        case Bytecode_Opcode::LOAD_A: {
            Interpreter_Register alloc_register = interpreter_load_register(interp, instruction.a);
            assert(alloc_register.pointer);

            assert(instruction.dest.kind == Bytecode_Register_Kind::TEMPORARY);

            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);
            interpreter_load_pointer(interp, alloc_register.pointer, &dest_reg, alloc_register.type);

            interpreter_store_register(interp, dest_reg, instruction.dest);
            break;
        }

        case Bytecode_Opcode::STORE_PTR: {
            Interpreter_Register new_value = interpreter_load_register(interp, instruction.a);
            Interpreter_Register ptr_value = interpreter_load_register(interp, instruction.b);

            assert(ptr_value.type->kind == Type_Kind::POINTER);
            assert(new_value.type == ptr_value.type->pointer.base);

            interpreter_store_pointer(interp, new_value, ptr_value.value.pointer);
            break;
        }

        case Bytecode_Opcode::LOAD_PTR: {
            Interpreter_Register ptr_register = interpreter_load_register(interp, instruction.a);
            assert(ptr_register.type->kind == Type_Kind::POINTER);

            assert(ptr_register.value.pointer && "LOAD_PTR Attempting to load from a null pointer");

            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);
            interpreter_load_pointer(interp, ptr_register.value.pointer, &dest_reg, instruction.dest.type);

            interpreter_store_register(interp, dest_reg, instruction.dest);
            break;
        }

        case Bytecode_Opcode::INSERT_VALUE: {
            Type *struct_type = instruction.dest.type;
            assert(struct_type->flags & TYPE_FLAG_AGGREGATE);
            assert(struct_type->kind == Type_Kind::STRUCTURE);

            auto &member_types = struct_type->structure.member_types;
            assert(instruction.additional_index < member_types.count);

#ifndef NDEBUG
            Type *member_type = member_types[instruction.additional_index];
            assert(member_type == instruction.b.type);
#endif

            // @Cleanup: @TODO: @FIXME: alignment?
            assert(struct_type->bit_size % 8 == 0);
            auto size = struct_type->bit_size / 8;

            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);

            if (!dest_reg.pointer) {
                u8 *ptr = interpreter_stack_alloc(interp, size);
                dest_reg.pointer = ptr;
            }

            if (instruction.a.kind == Bytecode_Register_Kind::UNDEF) {
                memset(dest_reg.pointer, 0, size);
            } else {
                assert_msg(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY,
                           "[Interpreter] a register of INSERT_VALUE must be a temporary register or <undef>");

                // @TODO: @FIXME: When we do register allocation, we'll only have
                //                  to copy the old value over when we don't use
                //                  the same register...
                Interpreter_Register old_value = interpreter_load_register(interp, instruction.a);
                assert(old_value.type == struct_type);
                interpreter_store_pointer(interp, old_value, dest_reg.pointer);
            }

            u64 member_offset = 0;
            for (s64 i = 0; i < instruction.additional_index; i++) {
                // @Cleanup: @TODO: @FIXME: alignment?
                auto mem_type = member_types[i];
                assert(mem_type->bit_size % 8 == 0);
                member_offset += (mem_type->bit_size / 8);
            }

            u8 *elem_ptr = dest_reg.pointer + member_offset;

            Interpreter_Register new_value = interpreter_load_register(interp, instruction.b);
            interpreter_store_pointer(interp, new_value, elem_ptr);

            interpreter_store_register(interp, dest_reg, instruction.dest);

            break;
        }

        case Bytecode_Opcode::EXTRACT_VALUE: {
            Interpreter_Register agg_val = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_val = interpreter_load_register(interp, instruction.b);

            Type *agg_type = agg_val.type;
            assert(agg_type->flags & TYPE_FLAG_AGGREGATE);
            assert(agg_type->kind == Type_Kind::STRUCTURE);

            assert(index_val.type == &builtin_type_s32);
            s32 index = index_val.value.integer.s32;

            auto &member_types = agg_type->structure.member_types;
            assert(index >= 0 && index < member_types.count);

            assert(agg_val.pointer);
            u8 *ptr = agg_val.pointer;
            for (s64 i = 0; i < index; i++) {
                Type *member_type = member_types[i];
                // @Cleanup: @TODO: @FIXME: alignment?
                assert(member_type->bit_size % 8 == 0);
                ptr += (member_type->bit_size / 8);
            }

            Type *member_type = member_types[index];
            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);
            interpreter_load_pointer(interp, ptr, &dest_reg, member_type);

            interpreter_store_register(interp, dest_reg, instruction.dest);

            break;
        }

        case Bytecode_Opcode::INSERT_ELEMENT: {
            Type *array_type = instruction.dest.type;
            assert(array_type->kind == Type_Kind::STATIC_ARRAY);

            assert(instruction.additional_index >= 0);
            assert(instruction.additional_index < array_type->static_array.count);

            auto element_type = array_type->static_array.element_type;

            // @Cleanup: @TODO: @FIXME: alignment?
            assert(array_type->bit_size % 8 == 0);
            auto size = array_type->bit_size / 8;

            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);

            if (!dest_reg.pointer) {
                u8 *ptr = interpreter_stack_alloc(interp, size);
                dest_reg.pointer = ptr;
            }


            if (instruction.a.kind == Bytecode_Register_Kind::UNDEF) {
                memset(dest_reg.pointer, 0, size);
            } else {
                assert_msg(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY, "[Interpreter] a register of INSERT_ELEMENT must be a temporary register");

                // @TODO: @FIXME: When we do register allocation, we'll only have
                //                  to copy the old value over when we don't use
                //                  the same register...
                Interpreter_Register old_value = interpreter_load_register(interp, instruction.a);
                assert(old_value.type == array_type);
                interpreter_store_pointer(interp, old_value, dest_reg.pointer);
            }

            assert(element_type->bit_size % 8 == 0);
            u64 elem_offset = instruction.additional_index * (element_type->bit_size / 8);
            assert(elem_offset % 8 == 0);
            u8 *elem_ptr = dest_reg.pointer + elem_offset;

            Interpreter_Register new_value = interpreter_load_register(interp, instruction.b);
            interpreter_store_pointer(interp, new_value, elem_ptr);

            interpreter_store_register(interp, dest_reg, instruction.dest);

            break;
        }

        case Bytecode_Opcode::EXTRACT_ELEMENT: {
            Interpreter_Register array_val = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_val = interpreter_load_register(interp, instruction.b);

            Type *array_type = array_val.type;
            assert(array_type->kind == Type_Kind::STATIC_ARRAY);

            assert(index_val.type == &builtin_type_s64);
            auto index = index_val.value.integer.s64;

            auto element_type = array_type->static_array.element_type;
            assert(index >= 0);
            assert(index < array_type->static_array.count);

            assert(element_type->bit_size % 8 == 0);
            u64 elem_offset = index * (element_type->bit_size / 8);
            assert(elem_offset % 8 == 0);
            u8 *elem_ptr = array_val.pointer + elem_offset;

            Interpreter_Register dest_reg = interpreter_load_register(interp, instruction.dest);
            interpreter_load_pointer(interp, elem_ptr, &dest_reg, element_type);

            interpreter_store_register(interp, dest_reg, instruction.dest);

            break;
        }

        case Bytecode_Opcode::AGG_OFFSET_POINTER: {
            assert(instruction.a.kind == Bytecode_Register_Kind::ALLOC ||
                   instruction.a.kind == Bytecode_Register_Kind::GLOBAL ||
                   instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);

            Interpreter_Register agg_register = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_register = interpreter_load_register(interp, instruction.b);

            Type *agg_type = agg_register.type;
            if (instruction.a.kind == Bytecode_Register_Kind::TEMPORARY) {
                assert(instruction.a.type->kind == Type_Kind::POINTER);
                agg_type = agg_type->pointer.base;
            }

            if (agg_type->kind == Type_Kind::SLICE) {
                agg_type = agg_type->slice.struct_type;
            }

            assert(agg_type->flags & TYPE_FLAG_AGGREGATE);
            assert(agg_type->kind == Type_Kind::STRUCTURE);

            assert(index_register.type == &builtin_type_s32);
            s32 index = index_register.value.integer.s32;

            auto &member_types = agg_type->structure.member_types;
            assert(index >= 0 && index < member_types.count);

            u8 *ptr = nullptr;

            if (instruction.a.kind == Bytecode_Register_Kind::ALLOC ||
                instruction.a.kind == Bytecode_Register_Kind::GLOBAL) {
                ptr = agg_register.pointer;
            } else {
                assert(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);
                ptr = agg_register.value.pointer;
            }
            assert(ptr);

            for (s64 i = 0; i < index; i++) {
                Type *member_type = member_types[i];
                // @Cleanup: @TODO: @FIXME: alignment?
                assert(member_type->bit_size % 8 == 0);
                ptr += (member_type->bit_size / 8);
            }

            Type *member_type = member_types[index];
            Type *dest_type = get_pointer_type(member_type, &interp->context->ast_allocator);

            Interpreter_Register result = {
                .type = dest_type,
                .value = { .pointer = ptr },
            };

            interpreter_store_register(interp, result, instruction.dest);

            break;
        }

        case Bytecode_Opcode::ARR_OFFSET_POINTER: {
            assert(instruction.a.kind == Bytecode_Register_Kind::ALLOC ||
                   instruction.a.kind == Bytecode_Register_Kind::GLOBAL ||
                   instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);

            Interpreter_Register array_register = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_register = interpreter_load_register(interp, instruction.b);

            Type *array_type = array_register.type;
            if (instruction.a.kind == Bytecode_Register_Kind::TEMPORARY) {
                assert(instruction.a.type->kind == Type_Kind::POINTER);
                array_type = array_type->pointer.base;
            }

            assert(array_type->kind == Type_Kind::STATIC_ARRAY);

            assert(index_register.type == &builtin_type_s64);
            auto index = index_register.value.integer.s64;
            assert(index >= 0);
            assert(index < array_type->static_array.count);

            u8 *ptr = nullptr;

            if (instruction.a.kind == Bytecode_Register_Kind::ALLOC ||
                instruction.a.kind == Bytecode_Register_Kind::GLOBAL) {
                ptr = array_register.pointer;
            } else {
                assert(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);
                ptr = array_register.value.pointer;
            }
            assert(ptr);

            auto element_type = array_type->static_array.element_type;

            // @Cleanup: @TODO: @FIXME: alignment?
            assert(element_type->bit_size % 8 == 0);
            s64 offset = index * (element_type->bit_size / 8);
            // assert(offset % 8 == 0);
            ptr += offset;

            Type *dest_type = get_pointer_type(element_type, &interp->context->ast_allocator);

            Interpreter_Register result = {
                .type = dest_type,
                .value = { .pointer = ptr },
            };

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }

        case Bytecode_Opcode::PTR_OFFSET_POINTER: {
            assert(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);

            Interpreter_Register ptr_register = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_register = interpreter_load_register(interp, instruction.b);

            auto index = index_register.value.integer.s64;
            assert(index >= 0);

            u8 *ptr = ptr_register.value.pointer;

            assert_msg(ptr, "PTR_OFFSET_PTR on a null pointer");

            auto element_type = instruction.a.type->pointer.base;

            // @Cleanup: @TODO: @FIXME: alignment?
            assert(element_type->bit_size % 8 == 0);
            s64 offset = index * (element_type->bit_size / 8);
            // assert(offset % 8 == 0);
            ptr += offset;

            Interpreter_Register result = {
                .type = instruction.a.type,
                .value = { .pointer = ptr },
            };

            interpreter_store_register(interp, result, instruction.dest);
            break;
        }
        case Bytecode_Opcode::JMP: {
            assert(instruction.a.kind == Bytecode_Register_Kind::BLOCK);

            auto block_handle = instruction.a.block_handle;
            assert(block_handle >= 0 && block_handle < current_function->blocks.count);

            advance_ip = false;
            interp->jumped_from_block = frame->bp;
            frame->bp = block_handle;
            frame->ip = 0;

            break;
        }

        case Bytecode_Opcode::JMP_IF: {
            assert(instruction.b.kind == Bytecode_Register_Kind::BLOCK);
            assert(instruction.dest.kind == Bytecode_Register_Kind::BLOCK);

            Interpreter_Register cond_reg = interpreter_load_register(interp, instruction.a);
            assert(cond_reg.type->kind == Type_Kind::BOOLEAN);

            Bytecode_Block_Handle target_block_handle = -1;

            if (cond_reg.value.boolean) {
                target_block_handle = instruction.b.block_handle;
            } else {
                target_block_handle = instruction.dest.block_handle;
            }

            assert(target_block_handle >= 0 && target_block_handle < current_function->blocks.count);

            advance_ip = false;
            interp->jumped_from_block = frame->bp;
            frame->bp = target_block_handle;
            frame->ip = 0;
            break;
        }

        case Bytecode_Opcode::SWITCH: {
            assert(instruction.b.switch_handle.index < current_function->switches.count);
            auto bc_cases = current_function->switches[instruction.b.switch_handle.index];

            Bytecode_Block_Handle target_block_handle = bc_cases.default_or_post_block;

            Interpreter_Register value_reg = interpreter_load_register(interp, instruction.a);

            Bytecode_Block_Handle default_block_handle = -1;
            bool match = false;

            for (s64 i = 0; i < bc_cases.cases.count; i++) {

                if (!bc_cases.cases[i].is_default) {
                    Interpreter_Register case_value_reg = interpreter_load_register(interp, bc_cases.cases[i].case_val);

                    if (value_reg.value.integer.u64 == case_value_reg.value.integer.u64) {
                        target_block_handle = bc_cases.cases[i].block_register.block_handle;
                        match = true;
                        break;
                    }

                } else {
                    assert_msg(default_block_handle == -1, "Only one default block is allowed");
                    default_block_handle = bc_cases.cases[i].block_register.block_handle;
                }
            }

            if (!match && default_block_handle != -1) {
                assert(default_block_handle == target_block_handle);
            }

            assert(target_block_handle >= 0 && target_block_handle < current_function->blocks.count);

            advance_ip = false;
            interp->jumped_from_block = frame->bp;
            frame->bp = target_block_handle;
            frame->ip = 0;

            break;
        }

        case Bytecode_Opcode::PHI: {
            assert(instruction.a.phi_args_handle < current_function->phi_args.count);
            auto phi_args = current_function->phi_args[instruction.a.phi_args_handle];

            assert(interp->jumped_from_block != -1);
            assert(current_function->blocks.count > interp->jumped_from_block);

            Interpreter_Register result_register;

            auto from_block = interp->jumped_from_block;
            if (from_block == phi_args.true_block_handle) {
                result_register = interpreter_load_register(interp, phi_args.true_value);
            } else {
                assert(from_block == phi_args.false_block_handle);
                result_register = interpreter_load_register(interp, phi_args.false_value);
            }

            interpreter_store_register(interp, result_register, instruction.dest);
            break;
        }
    }

    if (advance_ip) {
        frame->ip++;
    }
}

u8 *interpreter_stack_alloc(Interpreter *interp, u64 size)
{
    auto block = interp->current_stack_block;

    assert(block->cap - block->used >= size);

    auto result = &block->data[block->used];
    block->used += size;

    stack_top_ptr(&interp->frames)->alloc_size += size;

    return result;
}

void interpreter_call_foreign_function(Interpreter *interp, Bytecode_Function_Handle fn_handle, s64 arg_count, s64 dest_index)
{
    assert(fn_handle >= 0 && fn_handle < interp->functions.count);
    auto foreign_fn = &interp->functions[fn_handle];
    assert(foreign_fn->flags & BC_FUNCTION_FLAG_FOREIGN);

    assert(foreign_fn->ffi_handle); // This should always exist for foreign functions

    interpreter_call_ffi(interp, foreign_fn->ffi_handle, arg_count, dest_index, foreign_fn->type);
}

void interpreter_call_pointer(Interpreter *interp, Bytecode_Register fn_reg, s64 arg_count, s64 dest_index)
{
    assert(fn_reg.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(fn_reg.type->kind == Type_Kind::FUNCTION);

    auto fn_type = fn_reg.type;
    fn_reg.type = get_pointer_type(fn_reg.type, &interp->context->ast_allocator);
    auto interp_fn_reg = interpreter_load_register(interp, fn_reg);

    FFI_Handle ffi_handle = interp_fn_reg.value.pointer;

    Bytecode_Function_Handle bc_fn_handle = ffi_find_callback(&interp->ffi, ffi_handle);
    if (bc_fn_handle !=  -1) {
        interpreter_push_stack_frame(interp, bc_fn_handle, arg_count, dest_index);
    } else {
        interpreter_call_ffi(interp, ffi_handle, arg_count, dest_index, fn_type);
    }

}

void interpreter_call_ffi(Interpreter *interp, FFI_Handle ffi_handle, s64 arg_count, s64 dest_index, Type *type)
{
    assert(type->kind == Type_Kind::FUNCTION);
    auto return_type = type->function.return_type;

    assert(stack_count(&interp->arg_stack) >= arg_count);

    ffi_reset(&interp->ffi);

    bool is_vararg = type->function.is_c_vararg;

    for (s64 i = 0; i < arg_count; i++) {
        Interpreter_Register arg_reg = stack_peek(&interp->arg_stack, (arg_count - 1) - i);
        void *arg_ptr = nullptr;

        switch (arg_reg.type->kind) {
            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::VOID: assert(false); break;
            case Type_Kind::UNSIZED_INTEGER: assert(false); break;

            case Type_Kind::INTEGER:
            case Type_Kind::POINTER:
            case Type_Kind::FUNCTION:
            case Type_Kind::ENUM: {
                arg_ptr = &arg_reg.value;
                break;
            }

            case Type_Kind::FLOAT: {
                if (arg_reg.type->bit_size == 32) {
                    arg_ptr = &arg_reg.value.real.r32;
                } else {
                    assert(arg_reg.type->bit_size == 64);
                    arg_ptr = &arg_reg.value.real.r64;
                }
                break;
            }

            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::STRUCTURE: assert(false); break;
            case Type_Kind::STATIC_ARRAY: assert(false); break;
            case Type_Kind::SLICE: assert(false); break;
        }

        assert(arg_ptr);

        double d;
        if (is_vararg && arg_reg.type == &builtin_type_r32) {
            arg_reg.type = &builtin_type_r64;
            d = *(float *)arg_ptr;
            arg_ptr = &d;
        }

        ffi_push_arg(&interp->ffi, arg_ptr, arg_reg.type);
    }

    if (arg_count) stack_pop(&interp->arg_stack, arg_count);

    auto frame = stack_top_ptr(&interp->frames);
    void *return_val_ptr = nullptr;
    if (dest_index >= 0) {
        assert(dest_index < frame->register_count);
        Interpreter_Register *dest_reg = &interp->registers[frame->register_start + dest_index];

        switch (dest_reg->type->kind) {
            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::UNSIZED_INTEGER: assert(false); break;

            case Type_Kind::VOID: break;

            case Type_Kind::INTEGER:
            case Type_Kind::FLOAT:
            case Type_Kind::POINTER: {
                return_val_ptr = &dest_reg->value;
                break;
            }

            case Type_Kind::FUNCTION: assert(false); break;
            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::STRUCTURE: assert(false); break;
            case Type_Kind::ENUM: assert(false); break;
            case Type_Kind::STATIC_ARRAY: assert(false); break;
            case Type_Kind::SLICE: assert(false); break;
        }

        assert(return_val_ptr || dest_reg->type->kind == Type_Kind::VOID);
    }

    ffi_call(&interp->ffi, ffi_handle, return_val_ptr, return_type);
}

Interpreter_Register *interpreter_handle_ffi_callback(Interpreter *interp, Bytecode_Function_Handle fn_handle)
{
   // Arguments are already transformed into the regular form on the ffi side.

   assert(fn_handle >= 0 && fn_handle < interp->functions.count);
   auto bc_func = &interp->functions[fn_handle];

   // Push a dummy stack frame for the return value
   auto return_type = bc_func->type->function.return_type;
   Interpreter_Register *return_value_reg = nullptr;
   if (return_type->kind != Type_Kind::VOID) {
       assert(!(return_type->flags & TYPE_FLAG_AGGREGATE));
       assert(interp->registers.count - interp->used_register_count > 0);

       Interpreter_Stack_Frame dummy_frame = {
           .ip = 0,
           .bp = 0,
           .fn_handle = -1,
           .register_start = interp->used_register_count,
           .register_count = 1,
           .dest_index = -1,
       };

       interp->used_register_count += 1;
       interp->registers[dummy_frame.register_start].type = return_type;
       return_value_reg = & interp->registers[dummy_frame.register_start];

       stack_push(&interp->frames, dummy_frame);
   }

   auto &arg_types = bc_func->type->function.parameter_types;
   auto sp = stack_count(&interp->frames);
   interpreter_push_stack_frame(interp, fn_handle, arg_types.count, 0);

   while (stack_count(&interp->frames) > sp) {
       auto instruction = interpreter_fetch_instruction(interp);
       interpreter_execute_instruction(interp, instruction);
   }

   return return_value_reg;
}

Interpreter_Register interpreter_load_register(Interpreter *interp, Bytecode_Register bc_reg)
{
    if (bc_reg.kind == Bytecode_Register_Kind::TEMPORARY ||
        bc_reg.kind == Bytecode_Register_Kind::ALLOC) {

        if (bc_reg.flags & BC_REGISTER_FLAG_LITERAL) {

            Interpreter_Register_Flags flags = INTERP_REG_FLAG_NONE;

            if ((bc_reg.type->flags & TYPE_FLAG_AGGREGATE) ||
                bc_reg.type->kind == Type_Kind::STATIC_ARRAY ||
                bc_reg.type->kind == Type_Kind::SLICE) {
                flags |= INTERP_REG_FLAG_AGGREGATE_LITERAL;
            }

            return { .flags = flags, .type = bc_reg.type, .value = bc_reg.value };

        } else {

            auto frame = stack_top_ptr(&interp->frames);
            assert(frame->fn_handle >= 0 && frame->fn_handle < interp->functions.count);

            assert(bc_reg.index < frame->register_count);
            auto interp_reg_ptr = &interp->registers[frame->register_start + bc_reg.index];
            return *interp_reg_ptr;
        }

    } else if (bc_reg.kind == Bytecode_Register_Kind::GLOBAL) {

        assert(bc_reg.index < interp->globals.count);
        auto interp_reg_ptr = &interp->globals[bc_reg.index];
        assert(interp_reg_ptr->type == bc_reg.type);
        return *interp_reg_ptr;

    } else if (bc_reg.kind == Bytecode_Register_Kind::ZEROINITIALIZER) {
        Interpreter_Register result { .flags = INTERP_REG_FLAG_ZEROINITIALIZER,
                                      .type = bc_reg.type };
        return result;

    } else if (bc_reg.kind == Bytecode_Register_Kind::FUNCTION) {

        assert(bc_reg.type->kind == Type_Kind::FUNCTION);

        Interpreter_Register result = {
            .type = bc_reg.type,
        };

        Bytecode_Function_Handle fn_handle = bc_reg.value.function_handle;
        assert(fn_handle >= 0 && fn_handle < interp->functions.count);

        Bytecode_Function *fn = &interp->functions[fn_handle];

        assert(fn->ffi_handle);
        result.value.pointer = (u8 *)fn->ffi_handle;

        return result;

    } else {
        assert(false);
    }

    assert(false);
    return {};
}

void interpreter_load_pointer(Interpreter *interp, u8 *source, Interpreter_Register *dest, Type *type)
{
    assert(interp);

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::ENUM: {
            type = type->enumeration.integer_type;
            // Falltrough
        }

        case Type_Kind::INTEGER: {
            switch (type->bit_size) {
                // @Cleanup: @TODO: @FIXME: alignment?
                default: assert(false && !"interpreter_load_pointer unhandled integer bit width"); break;
                case 8: dest->value.integer.u8 = *((u8 *)source); break;
                case 16: dest->value.integer.u16 = *((u16 *)source); break;
                case 32: dest->value.integer.u32 = *((u32 *)source); break;
                case 64: dest->value.integer.u64 = *((u64 *)source); break;
            }
            break;
        }

        case Type_Kind::FLOAT: {
            switch (type->bit_size) {
                // @Cleanup: @TODO: @FIXME: alignment?
                default: assert(false && !"interpreter_load_pointer unhandled integer bit width"); break;
                case 32: dest->value.real.r32 = *((float *)source); break;
                case 64: dest->value.real.r64 = *((double *)source); break;
            }
            break;
        }

        case Type_Kind::POINTER:
        case Type_Kind::FUNCTION: {
            assert(type->bit_size == pointer_size);

            // @Cleanup: @TODO: @FIXME: alignment?
            dest->value.pointer = (u8 *) *((void **)source); break;
            break;
        }

        case Type_Kind::BOOLEAN: {
            // @Cleanup: @TODO: @FIXME: alignment?
            dest->value.boolean = *((bool *)source);
            break;
        }

        case Type_Kind::STRUCTURE:
        case Type_Kind::STATIC_ARRAY: {
            // @Cleanup: @TODO: @FIXME: alignment?
            assert (type->bit_size % 8 == 0);
            auto size = type->bit_size / 8;
            if (!dest->pointer) {
                u8 *ptr = interpreter_stack_alloc(interp, size);

                dest->pointer = ptr;
            }

            zmemcpy(dest->pointer, source, size);
            break;
        }

        case Type_Kind::SLICE: {
            interpreter_load_pointer(interp, source, dest, type->slice.struct_type);
            break;
        }
    }
}

void interpreter_store_register(Interpreter *interp, Interpreter_Register source,
                                Bytecode_Register dest)
{
    assert(dest.type == source.type);

    auto frame = stack_top_ptr(&interp->frames);
    assert(dest.index <= frame->register_count);
    auto dest_ptr = &interp->registers[frame->register_start + dest.index];

    bool agg_lit = source.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL;

    assert(dest_ptr->type == source.type);

    if (dest.kind == Bytecode_Register_Kind::TEMPORARY) {

        assert(!(dest.flags & BC_REGISTER_FLAG_LITERAL));

#ifndef NDEBUG
        if (frame->fn_handle != -1) {
            auto fn = &interp->functions[frame->fn_handle];
            auto bc_dest_ptr = &fn->registers[dest.index];
            assert(!(bc_dest_ptr->flags & BC_REGISTER_FLAG_LITERAL));
        }
#endif

        Interpreter_Register_Flags flags = INTERP_REG_FLAG_NONE;
        if (agg_lit) {
            flags |= INTERP_REG_FLAG_AGGREGATE_LITERAL;
        }

        dest_ptr->value = source.value;
        dest_ptr->flags = flags;

    } else if (dest.kind == Bytecode_Register_Kind::ALLOC) {
        assert(!agg_lit);
        assert(source.pointer);
        dest_ptr->pointer = source.pointer;

    } else {
        assert(false);
    }
}

#if WIN32
#pragma warning(push)
#pragma warning(disable: 4100)
#endif

void interpreter_store_pointer(Interpreter* interp, Interpreter_Register source, u8 *dest)
{
    assert(interp);

    if (source.flags & INTERP_REG_FLAG_ZEROINITIALIZER) {
        zmemset(dest, 0, source.type->bit_size / 8);
        return;
    }

    auto t = source.type;
    switch (t->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;


        case Type_Kind::ENUM: {
            t = t->enumeration.integer_type;
            // Falltrough
        }

        case Type_Kind::INTEGER: {

            // @Cleanup: @TODO: @FIXME: alignment?
            switch (t->bit_size) {
                default: assert(false && !"interpreter_store_pointer integer bit width unhandled"); break;
                case 8: *((u8 *)dest) = source.value.integer.u8; break;
                case 16: *((u16 *)dest) = source.value.integer.u16; break;
                case 32: *((u32 *)dest) = source.value.integer.u32; break;
                case 64: *((u64 *)dest) = source.value.integer.u64; break;
            }
            break;
        }

        case Type_Kind::FLOAT: {

            // @Cleanup: @TODO: @FIXME: alignment?
            switch (t->bit_size) {
                default: assert(false && !"interpreter_store_pointer float bit width unhandled"); break;
                case 32: *((float *)dest) = source.value.real.r32; break;
                case 64: *((double *)dest) = source.value.real.r64; break;
            }
            break;
        }

        case Type_Kind::POINTER: {
            // @Cleanup: @TODO: @FIXME: alignment?
            *((u8 **)dest) = source.value.pointer;
            break;
        }

        case Type_Kind::FUNCTION: {
            // @Cleanup: @TODO: @FIXME: alignment?
            *((u8 **)dest) = source.value.pointer;
            break;
        }

        case Type_Kind::BOOLEAN: {
            // @Cleanup: @TODO: @FIXME: alignment?
            *((bool *)dest) = source.value.boolean;
            break;
        }

        case Type_Kind::STRUCTURE:
        case Type_Kind::STATIC_ARRAY:
        case Type_Kind::SLICE: {

            if (source.type->kind == Type_Kind::SLICE) {
                source.type = source.type->slice.struct_type;
            }

            if (source.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL) {
                interpreter_copy_compound_literal_into_memory(interp, dest, source);
            } else {
                // @Cleanup: @TODO: @FIXME: alignment?
                assert (t->bit_size % 8 == 0);
                auto size = t->bit_size / 8;

                zmemcpy(dest, source.pointer, size);
            }
            break;
        }
    }
}

#if WIN32
#pragma warning(pop)
#endif

void interpreter_push_stack_frame(Interpreter *interp, Bytecode_Function_Handle fn_handle,
                                  s64 arg_count, s64 result_index)
{
    assert(fn_handle >= 0 && fn_handle < interp->functions.count);
    assert(arg_count <= stack_count(&interp->arg_stack));

    auto fn = &interp->functions[fn_handle];
    assert(arg_count <= fn->registers.count);

    if (result_index >= 0) {
        assert(fn->type->function.return_type->kind != Type_Kind::VOID);
    }

    auto free_register_count = interp->registers.count - interp->used_register_count;
    if (free_register_count < fn->registers.count) {

        auto old_data = interp->registers.data;

        auto new_count = interp->registers.count * 2;
        while (new_count - interp->used_register_count < fn->registers.count) {
            new_count *= 2;
        }

        auto new_data = alloc_array<Interpreter_Register>(interp->allocator, new_count);
        zmemcpy(new_data, old_data, interp->registers.count * sizeof(Interpreter_Register));

        interp->registers = { new_data, new_count };

        free(interp->allocator, old_data);
    }

    auto free_stack_size = interp->current_stack_block->cap - interp->current_stack_block->used;
    if (free_stack_size < fn->required_stack_size) {

        Stack_Block *new_block = nullptr;

        if (interp->free_stack_blocks) {

            auto free_block = interp->free_stack_blocks;
            Stack_Block *last_checked = nullptr;
            while (free_block) {

                if (free_block->cap >= fn->required_stack_size) {

                    new_block = free_block;

                    if (new_block == interp->free_stack_blocks) {
                        interp->free_stack_blocks = new_block->previous;
                    } else {
                        assert(last_checked);
                        assert(last_checked->previous == new_block);
                        last_checked->previous = new_block->previous;
                    }
                    break;
                }

                last_checked = free_block;
                free_block = free_block->previous;
            }
        }

        if (!new_block) {

            auto new_cap = interp->current_stack_block->cap * 2;
            while (new_cap < fn->required_stack_size) {
                new_cap *= 2;
            }

            auto mem = alloc_array<u8>(interp->allocator, sizeof(Stack_Block) + new_cap);
            new_block = (Stack_Block *)mem;
            new_block->data = &mem[sizeof(Stack_Block)];
            new_block->cap = new_cap;
        }

        new_block->used = 0;
        new_block->previous = interp->current_stack_block;

        interp->current_stack_block = new_block;
    }

    Interpreter_Stack_Frame new_frame = {
        .ip = 0,
        .bp = 0,
        .fn_handle = fn_handle,
        .register_start = interp->used_register_count,
        .register_count = fn->registers.count,
        .alloc_size = 0,
        .stack_block = interp->current_stack_block,
        .dest_index = result_index,
    };

    if (fn->required_stack_size) {
        zmemset(&interp->current_stack_block->data[interp->current_stack_block->used], 0, fn->required_stack_size);
    }

    interp->used_register_count += fn->registers.count;
    if (fn->registers.count) {

        zmemset(&interp->registers[new_frame.register_start], 0, new_frame.register_count * sizeof(Interpreter_Register));

        for (s64 i = 0; i < new_frame.register_count; i++) {
            interp->registers[new_frame.register_start + i].type = fn->registers[i].type;
        }
    }

    stack_push(&interp->frames, new_frame);

    for (s64 i = 0; i < arg_count; i++) {
        Interpreter_Register arg_source = stack_peek(&interp->arg_stack, (arg_count - 1) - i);

        Bytecode_Register arg_dest = {
            .kind = Bytecode_Register_Kind::TEMPORARY,
            .flags = BC_REGISTER_FLAG_NONE,
            .index = i,
            .type = arg_source.type,
        };

        if (TYPE_IS_SLICE_STRUCT(arg_source.type) && fn->param_types[i]->kind == Type_Kind::SLICE) {
            assert(arg_source.type == fn->param_types[i]->slice.struct_type);
            arg_source.type = fn->param_types[i];
            arg_dest.type = fn->param_types[i];
        }

        interpreter_store_register(interp, arg_source, arg_dest);
    }

    if (arg_count) stack_pop(&interp->arg_stack, arg_count);
}

Interpreter_Stack_Frame interpreter_pop_stack_frame(Interpreter *interp)
{
    auto old_frame = stack_pop(&interp->frames);

#ifndef NDEBUG
    auto fn_handle = old_frame.fn_handle;
    assert(fn_handle >= 0 && fn_handle < interp->functions.count);
    auto fn = interp->functions[fn_handle];
#endif

    assert(old_frame.register_count == fn.registers.count);
    interp->used_register_count -= old_frame.register_count;

    old_frame.stack_block->used -= old_frame.alloc_size;

    if (stack_count(&interp->frames)) {
        auto old_block = old_frame.stack_block;
        auto current_block = stack_top_ptr(&interp->frames)->stack_block;

        if (current_block != old_block) {
            assert(old_block->previous == current_block);

            interp->current_stack_block = current_block;
            old_block->previous = interp->free_stack_blocks;
            interp->free_stack_blocks = old_block;
        }
    }

    return old_frame;
}

void interpreter_copy_compound_literal_into_memory(Interpreter *interp, u8 *dest, Interpreter_Register source)
{
    debug_assert(interp && dest);

    assert((source.type->flags & TYPE_FLAG_AGGREGATE) ||
           source.type->kind == Type_Kind::STATIC_ARRAY);

    assert(source.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL);

    auto compound_type = source.type;
    assert(compound_type->kind == Type_Kind::STRUCTURE ||
           compound_type->kind == Type_Kind::STATIC_ARRAY);

    bool is_array = compound_type->kind == Type_Kind::STATIC_ARRAY;

    Type *member_type = nullptr;

    if (is_array) {
        assert(compound_type->static_array.count == source.value.compound.count);
        member_type = compound_type->static_array.element_type;
    } else {
        assert(compound_type->structure.member_types.count == source.value.compound.count);
    }


    u8 *dest_cursor = dest;

    for (s64 cmi = 0; cmi < source.value.compound.count; cmi++) {
        if (!is_array) {
            member_type = compound_type->structure.member_types[cmi];
        }

        Bytecode_Register bc_mem_reg = source.value.compound[cmi];
        Interpreter_Register mem_reg;

        if (bc_mem_reg.kind == Bytecode_Register_Kind::GLOBAL ||
            bc_mem_reg.kind == Bytecode_Register_Kind::ALLOC) {

            assert(member_type->kind == Type_Kind::POINTER);
            mem_reg = interpreter_load_register(interp, bc_mem_reg);
            assert(mem_reg.pointer);
            if (mem_reg.type->kind == Type_Kind::STATIC_ARRAY) {
                assert(member_type->kind == Type_Kind::POINTER);
                assert(member_type->pointer.base == mem_reg.type->static_array.element_type);
                mem_reg.type = member_type;
            } else {
                assert(false);
            }

        } else {
            assert(member_type == bc_mem_reg.type);
            mem_reg = interpreter_load_register(interp, bc_mem_reg);
        }

        // @Cleanup: @TODO: @FIXME: alignment
        auto copy_size = mem_reg.type->bit_size / 8;

        auto mem_type = mem_reg.type;
        if (mem_type->kind == Type_Kind::ENUM) {
            mem_type = mem_type->enumeration.integer_type;
        }

        switch (mem_type->kind) {

            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::VOID: assert(false); break;


#define COPY_INT_CASE(size) case size: *((u##size*)dest_cursor) = mem_reg.value.integer.u##size; break;
            case Type_Kind::UNSIZED_INTEGER:
            case Type_Kind::INTEGER: {

                switch (mem_reg.type->bit_size) {
                    COPY_INT_CASE(8)
                    COPY_INT_CASE(16)
                    COPY_INT_CASE(32)
                    COPY_INT_CASE(64)
                    default: assert_msg(false, "Unsupported integer size");
                }

                break;
            }
#undef COPY_INT_CASE

            case Type_Kind::FLOAT: {
                switch (mem_reg.type->bit_size) {
                    default: assert_msg(false, "Unsupported real bit size"); break;
                    case 32: *((float*)dest_cursor) = mem_reg.value.real.r32; break;
                    case 64: *((double*)dest_cursor) = mem_reg.value.real.r64; break;
                }
                break;
            }

            case Type_Kind::BOOLEAN: {
                *((bool*)dest_cursor) = mem_reg.value.boolean;
                break;
            }

            case Type_Kind::POINTER:
            case Type_Kind::FUNCTION: {
                *((u8**)dest_cursor) = mem_reg.value.pointer;
                break;
            }

            case Type_Kind::STRUCTURE: {
                assert(mem_reg.type->flags & TYPE_FLAG_AGGREGATE);
                assert(mem_reg.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL);

                auto agg_mem_type = mem_reg.type;
                assert(agg_mem_type->kind == Type_Kind::STRUCTURE);
                assert(agg_mem_type->structure.member_types.count == mem_reg.value.compound.count);

                interpreter_copy_compound_literal_into_memory(interp, dest_cursor, mem_reg);
                break;
            }

            case Type_Kind::STATIC_ARRAY: {
                interpreter_copy_compound_literal_into_memory(interp, dest_cursor, mem_reg);
                break;
            }

            case Type_Kind::ENUM: assert(false); break;
            case Type_Kind::SLICE: assert(false); break;
        }

        // @Cleanup: @TODO: @FIXME: alignment?
        dest_cursor += copy_size;
    }
}

}}
