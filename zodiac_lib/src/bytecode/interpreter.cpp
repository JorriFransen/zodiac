#include "bytecode/interpreter.h"

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "platform/platform.h"
#include "type.h"
#include "util/asserts.h"
#include "util/zstring.h"
#include "zodiac_context.h"

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

    const auto stack_mem_size = MEBIBYTE(1);
    out_interp->stack_mem.data = alloc_array<u8>(allocator, stack_mem_size);
    out_interp->stack_mem.count = stack_mem_size;
    out_interp->stack_mem_used = 0;

    filesystem_stdout_file(&out_interp->std_out);

    out_interp->ffi = ffi_create(allocator, context, true, interpreter_handle_ffi_callback);
}

void interpreter_free(Interpreter *interp)
{
    stack_free(&interp->frames);
    stack_free(&interp->arg_stack);

    free(interp->allocator, interp->registers.data);
    free(interp->allocator, interp->stack_mem.data);

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

    interp->functions = functions;

    for (s64 i = 0; i < foreign_functions.count; i++) {

        auto ffn_handle = foreign_functions[i];
        assert(ffn_handle >= 0 && ffn_handle < interp->functions.count);
        auto ffn = &interp->functions[ffn_handle];
        assert(ffn->flags & BC_FUNCTION_FLAG_FOREIGN);

        FFI_Handle ffi_handle = ffi_load_function(&interp->ffi, &ffn->name);
        assert(ffi_handle);

        assert(ffn->ffi_handle == nullptr);
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
            Interpreter_Register glob_reg = {
                .type = globals[i].type,
                .pointer = glob_cur,
            };

            // @Cleanup: @TODO: @FIXME: alignment
            assert(globals[i].type->bit_size % 8 == 0);
            auto size = globals[i].type->bit_size / 8;
            glob_cur += size;

            if (globals[i].initial_value.kind != Bytecode_Register_Kind::INVALID) {

                assert(globals[i].initial_value.kind == Bytecode_Register_Kind::TEMPORARY);
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

#define INTEGER_BINOP_CMP_CASE(op) { \
Interpreter_Register lhs = interpreter_load_register(interp, instruction.a); \
Interpreter_Register rhs = interpreter_load_register(interp, instruction.b); \
assert(lhs.type == rhs.type); \
assert(lhs.type->kind == Type_Kind::INTEGER || lhs.type->kind == Type_Kind ::BOOLEAN); \
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

        case Bytecode_Opcode::PRINT: {
            Interpreter_Register operand = interpreter_load_register(interp, instruction.a);

            assert(operand.type->kind == Type_Kind::INTEGER ||
                   operand.type->kind == Type_Kind::FLOAT ||
                   operand.type->kind == Type_Kind::BOOLEAN ||
                   operand.type->kind == Type_Kind::POINTER ||
                   operand.type->kind == Type_Kind::STATIC_ARRAY ||
                   operand.type == &builtin_type_String);

            auto out_handle = (FILE *)interp->std_out.handle;

            if (operand.type == &builtin_type_String) {
                fprintf(out_handle, "%.*s", (int)operand.value.string.length, operand.value.string.data);
                break;
            }

            switch (operand.type->kind) {
                default: assert(false); break;

                case Type_Kind::INTEGER: {
                    if (operand.type->integer.sign) {
                        switch (operand.type->bit_size) {
                            default: assert(false); break;
                            case 8: fprintf(out_handle, "%d", operand.value.integer.s8); break;
                            case 16: fprintf(out_handle, "%d", operand.value.integer.s16); break;
                            case 32: fprintf(out_handle, "%d", operand.value.integer.s32); break;
                            case 64: fprintf(out_handle, "%lld", operand.value.integer.s64); break;
                        }
                    } else {
                        switch (operand.type->bit_size) {
                            default: assert(false); break;
                            case 8: fprintf(out_handle, "%u", operand.value.integer.u8); break;
                            case 16: fprintf(out_handle, "%u", operand.value.integer.u16); break;
                            case 32: fprintf(out_handle, "%u", operand.value.integer.u32); break;
                            case 64: fprintf(out_handle, "%llu", operand.value.integer.u64); break;
                        }
                    }
                    break;
                }

                case Type_Kind::FLOAT: {
                    switch (operand.type->bit_size) {
                        default: assert(false && !"Unhandled float bit size for print instruction"); break;
                        case 32: fprintf(out_handle, "%f", operand.value.real.r32); break;
                        case 64: fprintf(out_handle, "%f", operand.value.real.r64); break;
                    }
                    break;
                }

                case Type_Kind::BOOLEAN: {
                    fprintf(out_handle, "%s", operand.value.boolean ? "true" : "false");
                    break;
                }

                case Type_Kind::POINTER: {
                    fprintf(out_handle, "%p", operand.value.pointer);
                    break;
                }

                case Type_Kind::STATIC_ARRAY: {
                    fprintf(out_handle, "{ ");
                    u8 *cursor = operand.value.pointer;
                    auto elem_type = operand.type->static_array.element_type;
                    auto elem_size = elem_type->bit_size / 8;
                    for (s64 i = 0; i < operand.type->static_array.count; i++) {
                        if (i != 0) fprintf(out_handle, ", ");
                        interpreter_print_from_memory(interp, cursor, elem_type);
                        cursor += elem_size;
                    }
                    fprintf(out_handle, " }");
                    break;
                }
            }
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
            assert(instruction.a.type->kind == Type_Kind::POINTER);
            assert(instruction.a.type->pointer.base->kind == Type_Kind::FUNCTION);

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
                     return_value.type->flags & TYPE_FLAG_STATIC_ARRAY) {

                    if (return_value.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL) {
                        assert(return_value.type->bit_size % 8 == 0);
                        auto size = return_value.type->bit_size / 8;
                        u8 *ptr = new_frame->sp;
                        new_frame->sp += size;

                        interpreter_copy_compound_literal_into_memory(interp, ptr, return_value);
                        return_value = { .type = return_value.type, .pointer = ptr };

                    } else {
                        return_value = interpreter_load_pointer(interp, return_value.pointer, return_value.type);
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
            assert(frame->sp + size <= frame->stack_mem.data + frame->stack_mem.count);

            u8 *ptr = frame->sp;
            frame->sp += size;

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
            Interpreter_Register loaded_value = interpreter_load_pointer(interp, global_register.pointer, global_register.type);

            interpreter_store_register(interp, loaded_value, instruction.dest);
            break;
        }

        case Bytecode_Opcode::STORE_A: {
            Interpreter_Register new_value = interpreter_load_register(interp, instruction.a);
            Interpreter_Register alloc_register = interpreter_load_register(interp, instruction.b);

            assert(new_value.type == alloc_register.type);
            assert(alloc_register.pointer);

            interpreter_store_pointer(interp, new_value, alloc_register.pointer);
            break;
        }

        case Bytecode_Opcode::LOAD_A: {
            Interpreter_Register alloc_register = interpreter_load_register(interp, instruction.a);
            assert(alloc_register.pointer);

            assert(instruction.dest.kind == Bytecode_Register_Kind::TEMPORARY);
            Interpreter_Register loaded_value = interpreter_load_pointer(interp, alloc_register.pointer, alloc_register.type);

            interpreter_store_register(interp, loaded_value, instruction.dest);
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

            Interpreter_Register loaded_value = interpreter_load_pointer(interp, ptr_register.value.pointer, instruction.dest.type);

            interpreter_store_register(interp, loaded_value, instruction.dest);
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
            assert(frame->sp + size <= frame->stack_mem.data + frame->stack_mem.count);

            u8 *ptr = frame->sp;
            frame->sp += size;

            Interpreter_Register result_value = {
                .type = struct_type,
                .pointer = ptr,
            };

            if (instruction.a.kind == Bytecode_Register_Kind::UNDEF) {
                memset(result_value.pointer, 0, size);
            } else {
                assert_msg(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY,
                           "[Interpreter] a register of INSERT_VALUE must be a temporary register or <undef>");

                // @TODO: @FIXME: When we do register allocation, we'll only have
                //                  to copy the old value over when we don't use
                //                  the same register...
                Interpreter_Register old_value = interpreter_load_register(interp, instruction.a);
                assert(old_value.type == struct_type);
                interpreter_store_pointer(interp, old_value, result_value.pointer);
            }

            u64 member_offset = 0;
            for (s64 i = 0; i < instruction.additional_index; i++) {
                // @Cleanup: @TODO: @FIXME: alignment?
                auto mem_type = member_types[i];
                assert(mem_type->bit_size % 8 == 0);
                member_offset += (mem_type->bit_size / 8);
            }

            u8 *elem_ptr = result_value.pointer + member_offset;

            Interpreter_Register new_value = interpreter_load_register(interp, instruction.b);
            interpreter_store_pointer(interp, new_value, elem_ptr);

            interpreter_store_register(interp, result_value, instruction.dest);

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
            Interpreter_Register result = interpreter_load_pointer(interp, ptr, member_type);

            interpreter_store_register(interp, result, instruction.dest);

            break;
        }

        case Bytecode_Opcode::INSERT_ELEMENT: {
            Type *array_type = instruction.dest.type;
            assert(array_type->flags & TYPE_FLAG_STATIC_ARRAY);
            assert(array_type->kind == Type_Kind::STATIC_ARRAY);

            assert(instruction.additional_index >= 0);
            assert(instruction.additional_index < array_type->static_array.count);

            auto element_type = array_type->static_array.element_type;

            // @Cleanup: @TODO: @FIXME: alignment?
            assert(array_type->bit_size % 8 == 0);
            auto size = array_type->bit_size / 8;
            assert(frame->sp + size <= frame->stack_mem.data + frame->stack_mem.count);

            u8 *ptr = frame->sp;
            frame->sp += size;

            Interpreter_Register result_value = {
                .type = array_type,
                .pointer = ptr,
            };

            if (instruction.a.kind == Bytecode_Register_Kind::UNDEF) {
                memset(result_value.pointer, 0, size);
            } else {
                assert_msg(instruction.a.kind == Bytecode_Register_Kind::TEMPORARY, "[Interpreter] a register of INSERT_ELEMENT must be a temporary register");

                // @TODO: @FIXME: When we do register allocation, we'll only have
                //                  to copy the old value over when we don't use
                //                  the same register...
                Interpreter_Register old_value = interpreter_load_register(interp, instruction.a);
                assert(old_value.type == array_type);
                interpreter_store_pointer(interp, old_value, result_value.pointer);
            }

            assert(element_type->bit_size % 8 == 0);
            u64 elem_offset = instruction.additional_index * (element_type->bit_size / 8);
            assert(elem_offset % 8 == 0);
            u8 *elem_ptr = result_value.pointer + elem_offset;

            Interpreter_Register new_value = interpreter_load_register(interp, instruction.b);
            interpreter_store_pointer(interp, new_value, elem_ptr);

            interpreter_store_register(interp, result_value, instruction.dest);

            break;
        }

        case Bytecode_Opcode::EXTRACT_ELEMENT: {
            Interpreter_Register array_val = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_val = interpreter_load_register(interp, instruction.b);

            Type *array_type = array_val.type;
            assert(array_type->flags & TYPE_FLAG_STATIC_ARRAY);
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

            Interpreter_Register result = interpreter_load_pointer(interp, elem_ptr, element_type);

            interpreter_store_register(interp, result, instruction.dest);

            break;
        }

        case Bytecode_Opcode::AGG_OFFSET_POINTER: {
            assert(instruction.a.kind == Bytecode_Register_Kind::ALLOC ||
                   instruction.a.kind == Bytecode_Register_Kind::TEMPORARY);

            Interpreter_Register agg_register = interpreter_load_register(interp, instruction.a);
            Interpreter_Register index_register = interpreter_load_register(interp, instruction.b);

            Type *agg_type = agg_register.type;
            if (instruction.a.kind == Bytecode_Register_Kind::TEMPORARY) {
                assert(instruction.a.type->kind == Type_Kind::POINTER);
                agg_type = agg_type->pointer.base;
            }

            assert(agg_type->flags & TYPE_FLAG_AGGREGATE);
            assert(agg_type->kind == Type_Kind::STRUCTURE);

            assert(index_register.type == &builtin_type_s32);
            s32 index = index_register.value.integer.s32;

            auto &member_types = agg_type->structure.member_types;
            assert(index >= 0 && index < member_types.count);

            u8 *ptr = nullptr;

            if (instruction.a.kind == Bytecode_Register_Kind::ALLOC) {
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

            assert(array_type->flags & TYPE_FLAG_STATIC_ARRAY);
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
            assert(offset % 8 == 0);
            ptr += offset;

            Type *dest_type = get_pointer_type(element_type, &interp->context->ast_allocator);

            Interpreter_Register result = {
                .type = dest_type,
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

void interpreter_call_foreign_function(Interpreter *interp, Bytecode_Function_Handle fn_handle, s64 arg_count, s64 dest_index)
{
    assert(fn_handle >= 0 && fn_handle < interp->functions.count);
    auto foreign_fn = &interp->functions[fn_handle];
    assert(foreign_fn->flags & BC_FUNCTION_FLAG_FOREIGN);

    assert(foreign_fn->ffi_handle); // This should always exist for foreign functions

    interpreter_call_ffi(interp, foreign_fn->ffi_handle, arg_count, dest_index, foreign_fn->type->function.return_type);
}

void interpreter_call_pointer(Interpreter *interp, Bytecode_Register fn_ptr_reg_, s64 arg_count, s64 dest_index)
{
    assert(fn_ptr_reg_.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(fn_ptr_reg_.type->kind == Type_Kind::POINTER);
    assert(fn_ptr_reg_.type->pointer.base->kind == Type_Kind::FUNCTION);
    auto fn_ptr_reg = interpreter_load_register(interp, fn_ptr_reg_);
    assert(fn_ptr_reg.type->kind == Type_Kind::POINTER);
    assert(fn_ptr_reg.type->pointer.base->kind == Type_Kind::FUNCTION);


    FFI_Handle ffi_handle = fn_ptr_reg.value.pointer;

    Bytecode_Function_Handle bc_fn_handle = ffi_find_callback(&interp->ffi, ffi_handle);
    if (bc_fn_handle !=  -1) {
        interpreter_push_stack_frame(interp, bc_fn_handle, arg_count, dest_index);
    } else {
        auto return_type = fn_ptr_reg.type->pointer.base->function.return_type;
        interpreter_call_ffi(interp, ffi_handle, arg_count, dest_index, return_type);
    }

}

void interpreter_call_ffi(Interpreter *interp, FFI_Handle ffi_handle, s64 arg_count, s64 dest_index, Type *return_type)
{
    assert(stack_count(&interp->arg_stack) >= arg_count);

    ffi_reset(&interp->ffi);

    for (s64 i = 0; i < arg_count; i++) {
        Interpreter_Register arg_reg = stack_peek(&interp->arg_stack, (arg_count - 1) - i);
        void *arg_ptr = nullptr;

        switch (arg_reg.type->kind) {
            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::VOID: assert(false); break;
            case Type_Kind::UNSIZED_INTEGER: assert(false); break;

            case Type_Kind::INTEGER:
            case Type_Kind::FLOAT:
            case Type_Kind::POINTER: {
                arg_ptr = &arg_reg.value;
                break;
            }

            case Type_Kind::FUNCTION: assert(false); break;
            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::STRUCTURE: assert(false); break;
            case Type_Kind::STATIC_ARRAY: assert(false); break;
        }

        assert(arg_ptr);

        ffi_push_arg(&interp->ffi, arg_ptr, arg_reg.type);
    }

    if (arg_count) stack_pop(&interp->arg_stack, arg_count);

    auto frame = stack_top_ptr(&interp->frames);
    void *return_val_ptr = nullptr;
    if (dest_index >= 0) {
        Interpreter_Register *dest_reg = &frame->registers[dest_index];

        switch (dest_reg->type->kind) {
            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::UNSIZED_INTEGER: assert(false); break;

            case Type_Kind::VOID: break;

            case Type_Kind::INTEGER:
            case Type_Kind::FLOAT: {
                return_val_ptr = &dest_reg->value;
                break;
            }

            case Type_Kind::POINTER: assert(false); break;
            case Type_Kind::FUNCTION: assert(false); break;
            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::STRUCTURE: assert(false); break;
            case Type_Kind::STATIC_ARRAY: assert(false); break;
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
           .registers = Array_Ref<Interpreter_Register>(&interp->registers[interp->used_register_count], 1),
           .stack_mem = {},
           .sp = nullptr,
           .dest_index = -1,
       };

       interp->used_register_count += 1;
       dummy_frame.registers[0].type = return_type;
       return_value_reg = &dummy_frame.registers[0];

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

Interpreter_Register interpreter_load_register(Interpreter *interp,
                                               Bytecode_Register bc_reg)
{
    if (bc_reg.kind == Bytecode_Register_Kind::TEMPORARY ||
        bc_reg.kind == Bytecode_Register_Kind::ALLOC) {

        if (bc_reg.flags & BC_REGISTER_FLAG_LITERAL) {

            Interpreter_Register_Flags flags = INTERP_REG_FLAG_NONE;

            if ((bc_reg.type->flags & TYPE_FLAG_AGGREGATE) ||
                (bc_reg.type->flags & TYPE_FLAG_STATIC_ARRAY)) {
                flags |= INTERP_REG_FLAG_AGGREGATE_LITERAL;
            }

            return { .flags = flags, .type = bc_reg.type, .value = bc_reg.value };

        } else {

            auto frame = stack_top_ptr(&interp->frames);
            assert(frame->fn_handle >= 0 && frame->fn_handle < interp->functions.count);

            assert(bc_reg.index < frame->registers.count);
            auto interp_reg_ptr = &frame->registers[bc_reg.index];
            assert(interp_reg_ptr->type == bc_reg.type);
            return *interp_reg_ptr;
        }

    } else if (bc_reg.kind == Bytecode_Register_Kind::GLOBAL) {

        assert(bc_reg.index < interp->globals.count);
        auto interp_reg_ptr = &interp->globals[bc_reg.index];
        assert(interp_reg_ptr->type == bc_reg.type);
        return *interp_reg_ptr;

    } else {
        assert(false);
    }

    assert(false);
    return {};
}

Interpreter_Register interpreter_load_pointer(Interpreter *interp, u8 *source, Type *type)
{
    assert(interp);

    Interpreter_Register result = {
        .type = type,
    };

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            switch (type->bit_size) {
                // @Cleanup: @TODO: @FIXME: alignment?
                default: assert(false && !"interpreter_load_pointer unhandled integer bit width"); break;
                case 8: result.value.integer.u8 = *((u8 *)source); break;
                case 16: result.value.integer.u16 = *((u16 *)source); break;
                case 32: result.value.integer.u32 = *((u32 *)source); break;
                case 64: result.value.integer.u64 = *((u64 *)source); break;
            }
            break;
        }

        case Type_Kind::FLOAT: {
            switch (type->bit_size) {
                // @Cleanup: @TODO: @FIXME: alignment?
                default: assert(false && !"interpreter_load_pointer unhandled integer bit width"); break;
                case 32: result.value.real.r32 = *((float *)source); break;
                case 64: result.value.real.r64 = *((double *)source); break;
            }
            break;
        }

        case Type_Kind::POINTER: {
            // @TODO: @CLEANUP: Remove the magic number, should probably be in compile_unit
            assert(type->bit_size == 64);

            // @Cleanup: @TODO: @FIXME: alignment?
            result.value.pointer = *((u8 **)source); break;
            break;
        }

        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::BOOLEAN: {
            // @Cleanup: @TODO: @FIXME: alignment?
            result.value.boolean = *((bool *)source);
            break;
        }

        case Type_Kind::STRUCTURE:
        case Type_Kind::STATIC_ARRAY: {
            auto frame = stack_top_ptr(&interp->frames);

            // @Cleanup: @TODO: @FIXME: alignment?
            assert (type->bit_size % 8 == 0);
            auto size = type->bit_size / 8;
            assert(frame->sp + size <= frame->stack_mem.data + frame->stack_mem.count);

            u8 *ptr = frame->sp;
            frame->sp += size;

            result.pointer = ptr;

            zmemcpy(ptr, source, size);
            break;
        }
    }

    return result;
}

void interpreter_store_register(Interpreter *interp, Interpreter_Register source,
                                Bytecode_Register dest)
{
    assert(dest.type == source.type);

    auto frame = stack_top_ptr(&interp->frames);
    assert(dest.index <= frame->registers.count);
    auto dest_ptr = &frame->registers[dest.index];

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

    auto t = source.type;
    switch (t->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

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

        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::BOOLEAN: {
            // @Cleanup: @TODO: @FIXME: alignment?
            *((bool *)dest) = source.value.boolean;
            break;
        }

        case Type_Kind::STRUCTURE:
        case Type_Kind::STATIC_ARRAY: {

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

#ifndef NDEBUG
    auto free_register_count = interp->registers.count - interp->used_register_count;
    assert(free_register_count >= fn->registers.count);

    auto free_stack_size = interp->stack_mem.count - interp->stack_mem_used;
    assert(free_stack_size >= fn->required_stack_size);
#endif

    Interpreter_Stack_Frame new_frame = {
        .ip = 0,
        .bp = 0,
        .fn_handle = fn_handle,
        .registers = Array_Ref<Interpreter_Register>(&interp->registers[interp->used_register_count], fn->registers.count),
        .stack_mem = {},
        .sp = nullptr,
        .dest_index = result_index,
    };

    if (fn->required_stack_size) {
        new_frame.stack_mem = Array_Ref<u8>(&interp->stack_mem[interp->stack_mem_used], fn->required_stack_size),
        new_frame.sp = &new_frame.stack_mem[0];
    }

    interp->used_register_count += fn->registers.count;
    interp->stack_mem_used += fn->required_stack_size;

    for (s64 i = 0; i < new_frame.registers.count; i++) {
        new_frame.registers[i].type = fn->registers[i].type;
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

    assert(old_frame.registers.count == fn.registers.count);
    interp->used_register_count -= old_frame.registers.count;

    assert(old_frame.stack_mem.count == fn.required_stack_size);
    interp->stack_mem_used -= old_frame.stack_mem.count;

    return old_frame;
}

void interpreter_copy_compound_literal_into_memory(Interpreter *interp, u8 *dest, Interpreter_Register source)
{
    debug_assert(interp && dest);

    assert((source.type->flags & TYPE_FLAG_AGGREGATE) ||
           (source.type->flags & TYPE_FLAG_STATIC_ARRAY));

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

        assert(member_type == bc_mem_reg.type);

        Interpreter_Register mem_reg = interpreter_load_register(interp, bc_mem_reg);
        auto copy_size = mem_reg.type->bit_size / 8;

        switch (mem_reg.type->kind) {

            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::VOID: assert(false); break;


#define COPY_INT_CASE(size) case size: zmemcpy(dest_cursor, &mem_reg.value.integer.u##size, copy_size); break;
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

            case Type_Kind::FLOAT: assert(false); break;
            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::POINTER: assert(false); break;

            case Type_Kind::STRUCTURE: {
                assert(mem_reg.type->flags & TYPE_FLAG_AGGREGATE);
                assert(mem_reg.flags & INTERP_REG_FLAG_AGGREGATE_LITERAL);

                auto agg_mem_type = mem_reg.type;
                assert(agg_mem_type->kind == Type_Kind::STRUCTURE);
                assert(agg_mem_type->structure.member_types.count == mem_reg.value.compound.count);

                interpreter_copy_compound_literal_into_memory(interp, dest_cursor, mem_reg);
                break;
            }

            case Type_Kind::STATIC_ARRAY: assert(false); break;
            case Type_Kind::FUNCTION: assert(false); break;
        }

        // @Cleanup: @TODO: @FIXME: alignment?
        dest_cursor += copy_size;
    }
}

void interpreter_print_from_memory(Interpreter *interp, u8* mem, Type *type)
{
    debug_assert(interp && mem && type);

    auto out_handle = (FILE *)interp->std_out.handle;

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            if (type->integer.sign) {
                switch (type->bit_size) {
                    default: assert(false);
                    case 8: fprintf(out_handle, "%i", *(s8*)mem); break;
                    case 16: fprintf(out_handle, "%i", *(s16*)mem); break;
                    case 32: fprintf(out_handle, "%i", *(s32*)mem); break;
                    case 64: fprintf(out_handle, "%lli", *(s64*)mem); break;
                }
            } else {
                switch (type->bit_size) {
                    default: assert(false);
                    case 8: fprintf(out_handle, "%i", *(u8*)mem); break;
                    case 16: fprintf(out_handle, "%i", *(u16*)mem); break;
                    case 32: fprintf(out_handle, "%i", *(u32*)mem); break;
                    case 64: fprintf(out_handle, "%lli", *(u64*)mem); break;
                }
            }
            break;
        }

        case Type_Kind::FLOAT: assert(false); break;
        case Type_Kind::BOOLEAN: assert(false); break;
        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::STRUCTURE: assert(false); break;
        case Type_Kind::STATIC_ARRAY: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
    }
}

}}
