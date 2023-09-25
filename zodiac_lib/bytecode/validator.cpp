#include "bytecode/validator.h"

#include <stdarg.h>
#include <stdio.h>

#include "atom.h"
#include "common.h"
#include "containers/hash_table.h"
#include "containers/stack.h"
#include "memory/temporary_allocator.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/string_builder.h"
#include "zodiac_context.h"

namespace Zodiac { namespace Bytecode {

void bytecode_validator_init(Zodiac_Context *context, Allocator *allocator, Bytecode_Validator *validator, Array_Ref<Bytecode_Function> functions, Hash_Table<Bytecode_Instruction_Handle, Source_Pos> *instruction_locations)
{
    *validator = {};
    validator->context = context;
    validator->allocator = allocator;
    bytecode_visitor_init(allocator, &validator->visitor, validator, functions, instruction_locations);
    validator->visitor.visit_function = validate_function;
    validator->visitor.visit_instruction = validate_instruction;

    dynamic_array_create(allocator, &validator->errors);
}

void bytecode_validator_free(Bytecode_Validator *validator)
{
    dynamic_array_free(&validator->errors);
    bytecode_visitor_free(&validator->visitor);
}

void bytecode_validator_print_errors(Bytecode_Validator *validator)
{
    for (s64 i = 0; i < validator->errors.count; i++) {

        if (i != 0) fprintf(stderr, "\n");

        Validation_Error &ve = validator->errors[i];

        auto err = validator->context->errors[ve.error_handle];

        auto fn = &validator->visitor.functions[ve.instruction_handle.fn_index];
        auto block = &fn->blocks[ve.instruction_handle.block_index];

        fprintf(stderr, "%.*s:%lld:%lld: ",
                (int)err.source_range.start.name.length, err.source_range.start.name.data,
                err.source_range.start.line, err.source_range.start.index_in_line);

        fprintf(stderr, "%.*s\n\tIn function: '%.*s'\n\tIn block: '%.*s'\n\tIndex in block: %lld\n",
                (int)err.message.length, err.message.data,
                (int)fn->name.length, fn->name.data,
                (int)block->name.length, block->name.data,
                ve.instruction_handle.instruction_index);
    }
}

void bytecode_validator_report_error(Bytecode_Validator *validator, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    bytecode_validator_report_error(validator, validator->visitor.current_pos, fmt, args);

    va_end(args);
}

void bytecode_validator_report_error(Bytecode_Validator *validator, Bytecode_Instruction_Handle location, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    Source_Pos pos;
    if (validator->visitor.instruction_locations) {
        bool found = hash_table_find(validator->visitor.instruction_locations, location, &pos);
        assert_msg(found, "Did not find location for Bytecode_Instruction_Handle");
    } else {
        Source_Pos dummy = {
            .name = "<no_source>",
            .line = 0,
            .index_in_line = 0,
        };
        pos = dummy;
    }


    auto err_handle = zodiac_report_error(validator->context, Zodiac_Error_Kind::ZODIAC_BC_VALIDATION_ERROR, pos, fmt, args);

    Validation_Error ve {
        .instruction_handle = location,
        .error_handle = err_handle,
    };

    dynamic_array_append(&validator->errors, ve);

    va_end(args);
}

bool validate_bytecode(Bytecode_Validator *validator)
{
    return visit_bytecode(&validator->visitor);
}

bool validate_function(Bytecode_Visitor *visitor, Bytecode_Function_Handle fn_handle)
{
    assert(visitor->user_data);
    Bytecode_Validator *validator = static_cast<Bytecode_Validator *>(visitor->user_data);
    assert(validator);

    if (validator->context->options.trace_bytecode_validation) {
        auto fn = &visitor->functions[fn_handle];
        ZTRACE("Validating bytecode function: '%s'", fn->name.data);
    }

    auto err_count = validator->errors.count;

    assert(fn_handle >= 0 && fn_handle < visitor->functions.count);
    auto function = &visitor->functions[fn_handle];

    bool result = validate_function(validator, fn_handle);
    if (!result && err_count == validator->errors.count) {
        bytecode_validator_report_error(validator, "Validate function returned false for '%s', but no error has been reported...", function->name);
    }

    if (validator->context->options.trace_bytecode_validation)
        ZTRACE("\t%s", result ? "OK" : "FAIL");

    return result;
}

bool validate_function(Bytecode_Validator *validator, Bytecode_Function_Handle fn_handle)
{
    assert(fn_handle >= 0 && fn_handle < validator->visitor.functions.count);
    auto function = &validator->visitor.functions[fn_handle];

    if (function->flags & BC_FUNCTION_FLAG_FOREIGN) {
        return true;
    }

    for (s64 bi = 0; bi < function->blocks.count; bi++) {
        auto block = &function->blocks[bi];
        bool terminates = false;

        for (s64 ii = 0; ii < block->instructions.count; ii++) {
            auto inst = &block->instructions[ii];

            if (terminates) {
                Bytecode_Instruction_Handle i_handle = { .fn_index = fn_handle, .block_index = bi, .instruction_index = ii };
                bytecode_validator_report_error(validator, i_handle, "Terminator found in middle of block");
                return false;
                break;
            }

            terminates = bytecode_instruction_is_terminator(inst);
        }
    }

    auto nodes = validator_build_block_graph(function);

    // auto dot = block_graph_to_dot(nodes, function, temp_allocator_allocator());
    // printf("%.*s\n", (int)dot.length, dot.data);

    bool noreturn = function->flags & BC_FUNCTION_FLAG_NORETURN;

    // There is an exit node at the end, but the rest of the nodes indexes should
    //   match the corresponding block handle.
    assert(nodes.count == function->blocks.count + 1);

    if (!noreturn) {
        for (s32 i = function->blocks.count - 1; i >= 0; i--) {
            if (!nodes[i].returns) {
                assert(nodes[i].block);
                Bytecode_Instruction_Handle location = {
                    .fn_index = fn_handle,
                    .block_index = i,
                    .instruction_index = (s64)nodes[i].block->instructions.count - 1,
                };
                bytecode_validator_report_error(validator, location, "Not all control paths return a value");
                return false;
            }
        }
    }

    return true;
}

bool validate_instruction(Bytecode_Visitor *visitor, Bytecode_Instruction *instruction)
{
    assert(visitor->user_data);
    Bytecode_Validator *validator = static_cast<Bytecode_Validator *>(visitor->user_data);
    assert(validator);

    auto err_count = validator->errors.count;

    bool result = validate_instruction(validator, instruction);
    if (!result && err_count == validator->errors.count) {
        bytecode_validator_report_error(validator, "Validate instruction returned false for '%s', but no error has been reported...", Bytecode_Opecode_Names[(int)instruction->op]);
    }

    return result;
}

bool validate_instruction(Bytecode_Validator *validator, Bytecode_Instruction *instruction)
{
    auto visitor = &validator->visitor;

    switch (instruction->op) {
        case Bytecode_Opcode::NOP: assert(false); break;

#define VALIDATE_INTEGER_BINOP(op) case Bytecode_Opcode::op: { \
    if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->a.type->kind != Type_Kind::INTEGER) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be of integer type"); \
        return false; \
    } \
 \
    if (instruction->b.type->kind != Type_Kind::INTEGER) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be of integer type"); \
        return false; \
    } \
 \
    if (instruction->dest.type->kind != Type_Kind::INTEGER) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be of integer type"); \
        return false; \
    } \
 \
    if (instruction->a.type != instruction->b.type) { \
        bytecode_validator_report_error(validator, "The 'a' and 'b' register for '" #op "' must be of the same integer type"); \
        return false; \
    } \
 \
    if (instruction->a.type != instruction->dest.type) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be of the same integer type as the argument registers"); \
        return false; \
    } \
 \
    if (instruction->a.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->a.index != -1) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->a.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    if (instruction->b.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->b.index != -1) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->b.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    return true; \
    break; \
}

        VALIDATE_INTEGER_BINOP(I_ADD)
        VALIDATE_INTEGER_BINOP(I_SUB)
        VALIDATE_INTEGER_BINOP(I_MUL)
        VALIDATE_INTEGER_BINOP(I_DIV)

#undef VALIDATE_INTEGER_BINOP

#define VALIDATE_INTEGER_CMP_BINOP(op) case Bytecode_Opcode::op: {\
    if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->a.type->kind != Type_Kind::INTEGER && \
        instruction->a.type->kind != Type_Kind::BOOLEAN) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be of integer or boolean type"); \
        return false; \
    } \
 \
    if (instruction->b.type->kind != Type_Kind::INTEGER && \
        instruction->b.type->kind != Type_Kind::BOOLEAN ) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be of integer or boolean type"); \
        return false; \
    } \
 \
    if (instruction->dest.type->kind != Type_Kind::BOOLEAN) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be of boolean type"); \
        return false; \
    } \
 \
    if (instruction->a.type != instruction->b.type) { \
        bytecode_validator_report_error(validator, "The 'a' and 'b' register for '" #op "' must be of the same integer type"); \
        return false; \
    } \
 \
    if (instruction->a.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->a.index != -1) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->a.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    if (instruction->b.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->b.index != -1) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->b.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    return true; \
    break; \
}

        VALIDATE_INTEGER_CMP_BINOP(I_EQ)
        VALIDATE_INTEGER_CMP_BINOP(I_NEQ)
        VALIDATE_INTEGER_CMP_BINOP(I_GT)
        VALIDATE_INTEGER_CMP_BINOP(I_LT)
        VALIDATE_INTEGER_CMP_BINOP(I_GT_EQ)
        VALIDATE_INTEGER_CMP_BINOP(I_LT_EQ)

#undef VALIDATE_INTEGER_CMP_BINOP

#define VALIDATE_FLOAT_BINOP(op) case Bytecode_Opcode::op: { \
    if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->a.type->kind != Type_Kind::FLOAT) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be of float type"); \
        return false; \
    } \
 \
    if (instruction->b.type->kind != Type_Kind::FLOAT) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be of float type"); \
        return false; \
    } \
 \
    if (instruction->dest.type->kind != Type_Kind::FLOAT) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be of float type"); \
        return false; \
    } \
 \
    if (instruction->a.type != instruction->b.type) { \
        bytecode_validator_report_error(validator, "The 'a' and 'b' register for '" #op "' must be of the same float type"); \
        return false; \
    } \
 \
    if (instruction->a.type != instruction->dest.type) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be of the same float type as the argument registers"); \
        return false; \
    } \
 \
    if (instruction->a.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->a.index != -1) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->a.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    if (instruction->b.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->b.index != -1) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->b.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    return true; \
    break; \
}

        VALIDATE_FLOAT_BINOP(F_ADD)
        VALIDATE_FLOAT_BINOP(F_SUB)
        VALIDATE_FLOAT_BINOP(F_MUL)
        VALIDATE_FLOAT_BINOP(F_DIV)

#undef VALIDATE_FLOAT_BINOP

#define VALIDATE_FLOAT_CMP_BINOP(op) case Bytecode_Opcode::op: {\
    if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be a temporary register"); \
        return false; \
    } \
 \
    if (instruction->a.type->kind != Type_Kind::FLOAT) { \
        bytecode_validator_report_error(validator, "The 'a' register for '" #op "' must be of float type"); \
        return false; \
    } \
 \
    if (instruction->b.type->kind != Type_Kind::FLOAT) { \
        bytecode_validator_report_error(validator, "The 'b' register for '" #op "' must be of float type"); \
        return false; \
    } \
 \
    if (instruction->dest.type->kind != Type_Kind::BOOLEAN) { \
        bytecode_validator_report_error(validator, "The 'dest' register for '" #op "' must be of boolean type"); \
        return false; \
    } \
 \
    if (instruction->a.type != instruction->b.type) { \
        bytecode_validator_report_error(validator, "The 'a' and 'b' register for '" #op "' must be of the same float type"); \
        return false; \
    } \
 \
    if (instruction->a.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->a.index != -1) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->a.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'a' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    if (instruction->b.flags & BC_REGISTER_FLAG_LITERAL) { \
        if (instruction->b.index != -1) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal, but it's index is not -1"); \
            return false; \
        } \
 \
        if (instruction->b.flags & BC_REGISTER_FLAG_ARGUMENT) { \
            bytecode_validator_report_error(validator, "The 'b' register for '" #op "' is flagged as literal and argument"); \
            return false; \
        } \
    } \
 \
    return true; \
    break; \
}


        VALIDATE_FLOAT_CMP_BINOP(F_EQ)
        VALIDATE_FLOAT_CMP_BINOP(F_NEQ)
        VALIDATE_FLOAT_CMP_BINOP(F_GT)
        VALIDATE_FLOAT_CMP_BINOP(F_LT)
        VALIDATE_FLOAT_CMP_BINOP(F_GT_EQ)
        VALIDATE_FLOAT_CMP_BINOP(F_LT_EQ)

#undef VALIDATE_FLOAT_CMP_BINOP

        case Bytecode_Opcode::SQRT: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'SQRT' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::INTEGER &&
                instruction->a.type->kind != Type_Kind::FLOAT) {
                bytecode_validator_report_error(validator, "The 'a' register for 'SQRT' must be of float or integer type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'SQRT' must be a temporary");
                return false;
            }

            if (instruction->dest.type->kind != Type_Kind::INTEGER &&
                instruction->dest.type->kind != Type_Kind::FLOAT) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'SQRT' must of float or integer type");
                return false;
            }

            if (instruction->a.type != instruction->dest.type) {
                bytecode_validator_report_error(validator, "The 'a' register type for 'SQRT' doest not match the 'dest' register type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::TRUNC: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'TRUNC' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::INTEGER) {
                bytecode_validator_report_error(validator, "The 'a' register for 'TRUNC' must be of integer type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'TRUNC' must be a temporary");
                return false;
            }

            if (instruction->dest.type->kind != Type_Kind::INTEGER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'TRUNC' must be of integer type");
            }

            auto target_type = instruction->dest.type;
            auto op_type = instruction->a.type;

            if (!(target_type->bit_size < op_type->bit_size)) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'TRUNC' must have a smaller bit_size than the 'a' register");
                return false;
            }

            return true;
        }

        case Bytecode_Opcode::SEXT: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'SEXT' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::INTEGER) {
                bytecode_validator_report_error(validator, "The 'a' register for 'SEXT' must be of integer type");
                return false;
            }

            if (!instruction->a.type->integer.sign) {
                bytecode_validator_report_error(validator, "The 'a' register for 'SEXT' must be of signed integer type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'SEXT' must be a temporary");
                return false;
            }

            if (instruction->dest.type->kind != Type_Kind::INTEGER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'SEXT' must be of integer type");
            }

            if (!instruction->dest.type->integer.sign) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'SEXT' must be of signed integer type");
                return false;
            }

            auto target_type = instruction->dest.type;
            auto op_type = instruction->a.type;

            if (!(target_type->bit_size > op_type->bit_size)) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ZEXT' must have a smaller bit_size than the 'a' register");
                return false;
            }

            return true;
        }

        case Bytecode_Opcode::ZEXT: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'ZEXT' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::INTEGER) {
                bytecode_validator_report_error(validator, "The 'a' register for 'ZEXT' must be of integer type");
                return false;
            }

            if (instruction->a.type->integer.sign) {
                bytecode_validator_report_error(validator, "The 'a' register for 'ZEXT' must be of unsigned integer type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ZEXT' must be a temporary");
                return false;
            }

            if (instruction->dest.type->kind != Type_Kind::INTEGER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ZEXT' must be of integer type");
            }

            if (instruction->dest.type->integer.sign) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ZEXT' must be of unsigned integer type");
            }

            auto target_type = instruction->dest.type;
            auto op_type = instruction->a.type;

            if (!(target_type->bit_size > op_type->bit_size)) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ZEXT' must have a smaller bit_size than the 'a' register");
                return false;
            }

            return true;
        }

        case Bytecode_Opcode::PRINT: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'PRINT' must be a temporary");
                return false;
            }

            auto t = instruction->a.type;
            assert(t);

            return true;
            break;
        }

        case Bytecode_Opcode::PUSH_ARG: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'PUSH_ARG' must be a temporary");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::CALL:
        case Bytecode_Opcode::CALL_FOREIGN: {

            if (instruction->a.kind != Bytecode_Register_Kind::FUNCTION) {
                bytecode_validator_report_error(validator, "The 'a' register for 'CALL_*' must be a function");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register for 'CALL_*' must be a temporary");
                return false;
            }

            if (instruction->b.type != &builtin_type_s64) {
                bytecode_validator_report_error(validator, "The 'b' register for 'CALL_*' must be of s64 type");
                return false;
            }

            Bytecode_Function_Handle fn_handle = instruction->a.value.function_handle;
            if (fn_handle < 0 || fn_handle > visitor->functions.count) {
                bytecode_validator_report_error(validator, "Invalid function handle for 'CALL_*': %d", fn_handle);
            }

            Bytecode_Function *fn = &visitor->functions[fn_handle];
            if (instruction->op == Bytecode_Opcode::CALL_FOREIGN && !(fn->flags & BC_FUNCTION_FLAG_FOREIGN)) {
                bytecode_validator_report_error(validator, "'CALL_*' is being called with a non foreign function");
                return false;
            }

            auto fn_arg_count = fn->arg_count;
            auto pushed_arg_count = stack_count(&visitor->arg_stack);

            if (fn_arg_count > pushed_arg_count) {
                bytecode_validator_report_error(validator, "'CALL_*' to '%s' expected %d args, got %d", fn->name, fn_arg_count, pushed_arg_count);
                return false;
            }

            bool arg_match = true;
            for (s64 i = 0; i < fn_arg_count; i++) {
                auto arg_reg = stack_peek_ptr(&visitor->arg_stack, (fn_arg_count - 1) - i);
                if (arg_reg->type != fn->param_types[i]) {
                    bytecode_validator_report_error(validator, "Mismatching type for argument %d", i);
                    arg_match = false;
                }

            }

            if (!arg_match) return false;

            if (instruction->dest.index != -1) {

                if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                    bytecode_validator_report_error(validator, "The 'dest' register for 'CALL_* must be a temporary");
                    return false;
                }

                if (instruction->dest.type != fn->type->function.return_type) {
                    bytecode_validator_report_error(validator, "The type of the 'dest' register for 'CALL_*' does not match the return type of the called function");
                    return false;
                }
            }

            return true;
            break;
        }

        case Bytecode_Opcode::CALL_PTR: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'CALL_PTR' must be a function");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::POINTER) {// ||
                // instruction->a.type->pointer.base->kind != Type_Kind::FUNCTION) {
                assert(false);

                bytecode_validator_report_error(validator, "The 'a' register for 'CALL_PTR' must be a of function pointer type");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register for 'CALL_PTR' must be a temporary");
                return false;
            }

            if (instruction->b.type != &builtin_type_s64) {
                bytecode_validator_report_error(validator, "The 'b' register for 'CALL_PTR' must be of s64 type");
                return false;
            }

            auto fn_type = instruction->a.type->pointer.base;
            assert(fn_type->kind == Type_Kind::FUNCTION);

            const auto &args = fn_type->function.parameter_types;
            auto fn_arg_count = args.count;
            auto pushed_arg_count = stack_count(&visitor->arg_stack);

            if (fn_arg_count > pushed_arg_count) {
                bytecode_validator_report_error(validator, "'CALL_PTR' expected %d args, got %d", fn_arg_count, pushed_arg_count);
                return false;
            }

            bool arg_match = true;
            for (s64 i = 0; i < fn_arg_count; i++) {
                auto arg_reg = stack_peek_ptr(&visitor->arg_stack, (fn_arg_count - 1) - i);
                if (arg_reg->type != args[i]) {
                    bytecode_validator_report_error(validator, "Mismatching type for argument %d", i);
                    arg_match = false;
                }

            }

            if (!arg_match) return false;

            if (instruction->dest.index != -1) {

                if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                    bytecode_validator_report_error(validator, "The 'dest' register for 'CALL_PTR' must be a temporary");
                    return false;
                }

                if (instruction->dest.type != fn_type->function.return_type) {
                    bytecode_validator_report_error(validator, "The type of the 'dest' register for 'CALL_PTR' does noet match the return type of the called function");
                    return false;
                }
            }

            return true;
            break;
        }

        case Bytecode_Opcode::RETURN_VOID: {
            if (instruction->a.index != -1) {
                bytecode_validator_report_error(validator, "Did not expect the 'a' register to be set for 'RETURN_VOID'");
                return false;
            }

            if (instruction->b.index != -1) {
                bytecode_validator_report_error(validator, "Did not expect the 'b' register to be set for 'RETURN_VOID'");
                return false;
            }

            if (instruction->dest.index != -1) {
                bytecode_validator_report_error(validator, "Did not expect the 'dest' register to be set for 'RETURN_VOID'");
                return false;
            }

            auto fn_return_type = validator->visitor.current_function->type->function.return_type;
            assert(fn_return_type);
            if (fn_return_type != &builtin_type_void) {
                auto return_type_str = type_to_string(temp_allocator_allocator(), fn_return_type);
                bytecode_validator_report_error(validator, "Invalid 'RETURN_VOID' in function with return type '%.*s'",
                                                (int)return_type_str.length, return_type_str.data);
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::RETURN: {

            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'RETURN' must be a temporary");
                return false;
            }

            assert(visitor->current_function);

            auto expected_return_type = visitor->current_function->type->function.return_type;
            if (expected_return_type != instruction->a.type) {
                auto ert_str = type_to_string(temp_allocator_allocator(), expected_return_type);
                bytecode_validator_report_error(validator, "The type of the 'a' register for 'RETURN' does not match the return type of the function it's in ('%.*s').",
                                                (int)ert_str.length, ert_str.data);
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::ALLOC: {
            if (instruction->a.kind != Bytecode_Register_Kind::TYPE) {
                bytecode_validator_report_error(validator, "The 'a' register of 'ALLOC' must be a type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::ALLOC) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'ALLOC' must be an alloc");
                return false;
            }

            if (instruction->a.type != instruction->dest.type) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register does not match the specified type for 'ALLOC'");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::ADDROF: {
            if (instruction->a.kind != Bytecode_Register_Kind::ALLOC &&
                instruction->a.kind != Bytecode_Register_Kind::GLOBAL) {
                bytecode_validator_report_error(validator, "The 'a' register for 'ADDROF' must be an alloc or global");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ADDROF' must be a temporary");
                return false;
            }

            if (instruction->dest.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ADDROF' must be of pointer type");
                return false;
            }

            if (instruction->dest.type->pointer.base != instruction->a.type) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register for 'ADDROF' must be a pointer to the 'a' registers type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::ADDROF_FUNC:
        {
            if (instruction->a.kind != Bytecode_Register_Kind::FUNCTION) {
                bytecode_validator_report_error(validator, "The 'a' register for 'ADDROF_FUNC' must be a function");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::FUNCTION) {
                bytecode_validator_report_error(validator, "The 'a' register' for 'ADDROF_FUNC' must be of function type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ADDROF_FUNC' must be a temporary");
                return false;
            }

            if (!(instruction->dest.type->kind == Type_Kind::POINTER &&
                  instruction->dest.type->pointer.base->kind == Type_Kind::FUNCTION)) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ADDROF_FUNC' must be of function pointer type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::STORE_G: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register of 'STORE_G' must be a temporary");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::GLOBAL) {
                bytecode_validator_report_error(validator, "The 'b' register of 'STORE_G' must be a global");
                return false;
            }

            if (instruction->a.type != instruction->b.type) {
                bytecode_validator_report_error(validator, "The 'a' and 'b' registers for 'STORE_G' must have the same type");
                return false;
            }

            return true;
            break;
        }


        case Bytecode_Opcode::LOAD_G: {
            if (instruction->a.kind != Bytecode_Register_Kind::GLOBAL) {
                bytecode_validator_report_error(validator, "The 'a' register for 'LOAD_G' must be a global");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'LOAD_G' must be a temporary");
                return false;
            }

            if (instruction->a.type != instruction->dest.type) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'STORE_A' does not match the source type ('a' register)");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::STORE_A: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register of 'STORE_A' must be a temporary");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::ALLOC) {
                bytecode_validator_report_error(validator, "The 'b' register of 'STORE_A' must be an alloc");
                return false;
            }

            if (instruction->a.type != instruction->b.type) {
                bytecode_validator_report_error(validator, "The 'a' and 'b' registers for 'STORE_A' must have the same type.");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::LOAD_A: {
            if (instruction->a.kind != Bytecode_Register_Kind::ALLOC) {
                bytecode_validator_report_error(validator, "The 'a' register for 'LOAD_A' must be an alloc");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'LOAD_A' must be a temporary");
                return false;
            }

            if (instruction->a.type != instruction->dest.type) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'LOAD_A' does not match the source type ('a' register)");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::STORE_PTR: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'STORE_PTR' must be a temporary");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register for 'STORE_PTR' must be a temporary");
                return false;

            }

            if (instruction->b.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'b' register for 'STORE_PTR' must be of pointer type");
                return false;
            }

            if (instruction->a.type != instruction->b.type->pointer.base) {
                bytecode_validator_report_error(validator, "The type of the 'a' register for 'STORE_PTR' does not match the pointer-base type of the 'b' register");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::LOAD_PTR: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'LOAD_PTR' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'a' register for 'LOAD_PTR' must be of pointer type");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'LOAD_PTR' must be a temporary");
                return false;
            }

            if (instruction->dest.type != instruction->a.type->pointer.base) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register for 'LOAD_PTR' does not match the base type of the pointer type in the 'a' register");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::INSERT_VALUE: {

            Type *aggregate_type = nullptr;

            if (instruction->a.kind != Bytecode_Register_Kind::INVALID) {

                if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY &&
                    instruction->a.kind != Bytecode_Register_Kind::UNDEF) {
                    bytecode_validator_report_error(validator, "The 'a' register of 'INSERT_VALUE' must be a temporary or <undef>");

                    return false;
                }

                if (!(instruction->a.type->flags & TYPE_FLAG_AGGREGATE)) {
                    bytecode_validator_report_error(validator, "The 'a' register of 'INSERT_VALUE' must be an aggregate");
                    return false;
                }

                if (instruction->a.type->kind != Type_Kind::STRUCTURE) {
                    bytecode_validator_report_error(validator, "The 'a' register of 'INSERT_VALUE' must be of struct type");
                    return false;
                }

                aggregate_type = instruction->a.type;

            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register of 'INSERT_VALUE' must be a temporary");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'INSERT_VALUE' must be a temporary");
                return false;
            }

            Type *dest_type = instruction->dest.type;
            if (!(dest_type->flags & TYPE_FLAG_AGGREGATE)) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'INSERT_VALUE' is not of aggregate type");
                return 0;
            }

            if (dest_type->kind != Type_Kind::STRUCTURE) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'INSERT_VALUE' is not of struct type");
                return false;
            }

            if (aggregate_type && !(aggregate_type == dest_type)) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register  for 'INSERT_VALUE' does not match the type of the 'a' register");
                return false;
            }

            assert(dest_type->kind == Type_Kind::STRUCTURE);
            auto member_count = dest_type->structure.member_types.count;

            if (instruction->additional_index < 0 || instruction->additional_index > member_count) {
                bytecode_validator_report_error(validator, "Index for 'INSERT_VALUE' is not in range of the specified aggregate type");
                return false;
            }

            assert(dest_type->kind == Type_Kind::STRUCTURE);
            auto dest_elem_type = dest_type->structure.member_types[instruction->additional_index];

            if (dest_elem_type != instruction->b.type) {
                bytecode_validator_report_error(validator, "The 'b' registers type does noet match the aggregate member type at the specified index ('INSERT_VALUE')");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::EXTRACT_VALUE: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'EXTRACT_VALUE' must be a temporary");
                return false;
            }

            if (!(instruction->a.type->flags & TYPE_FLAG_AGGREGATE)) {
                bytecode_validator_report_error(validator, "The 'a' register for 'EXTRACT_VALUE' is not an aggregate");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::STRUCTURE) {
                bytecode_validator_report_error(validator, "The 'a' register for 'EXTRACT_VALUE' is not of struct type");
                return false;
            }

            Type *aggregate_type = instruction->a.type;

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register for 'EXTRACT_VALUE' must be a temporary");
            }

            if (instruction->b.type != &builtin_type_s32) {
                bytecode_validator_report_error(validator, "The 'b' register for 'EXTRACT_VALUE' does not have the expected integer type (s32)");
                return false;
            }

            if (!(instruction->b.flags & BC_REGISTER_FLAG_LITERAL)) {
                bytecode_validator_report_error(validator, "The 'b' register for 'EXTRACT_VALUE' must be a literal");
                return false;
            }

            auto index = instruction->b.value.integer.s32;

            assert(aggregate_type->kind == Type_Kind::STRUCTURE);
            if (index < 0 || index > aggregate_type->structure.member_types.count) {
                bytecode_validator_report_error(validator, "The index specified in the 'b' register for 'EXTRACT_ELEMENT' is out of range for the specified aggregate type");
                return false;
            }

            Type *result_type = aggregate_type->structure.member_types[index];

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'EXTRACT_VALUE' must be a temporary");
                return false;
            }

            if (result_type != instruction->dest.type) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'EXTRACT_VALUE' does not match the expected aggregate member type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::INSERT_ELEMENT: {

            Type *array_type = nullptr;

            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY &&
                instruction->a.kind != Bytecode_Register_Kind::UNDEF) {
                bytecode_validator_report_error(validator, "The 'a' register of 'INSERT_ELEMENT' must be a temporary or <undef>");

                return false;
            }

            if (instruction->a.type->kind != Type_Kind::STATIC_ARRAY) {
                bytecode_validator_report_error(validator, "The 'a' register of 'INSERT_ELEMENT' must be of static array type");
                return false;
            }

            array_type = instruction->a.type;

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register of 'INSERT_ELEMENT' must be a temporary");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'INSERT_ELEMENT' must be a temporary");
                return false;
            }

            Type *dest_type = instruction->dest.type;

            if (dest_type->kind != Type_Kind::STATIC_ARRAY) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'INSERT_ELEMENT' is not of static array type");
                return false;
            }

            if (array_type && !(array_type == dest_type)) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register  for 'INSERT_ELEMENT' does not match the type of the 'a' register");
                return false;
            }

            assert(dest_type->kind == Type_Kind::STATIC_ARRAY);
            auto elem_count = dest_type->static_array.count;

            if (instruction->additional_index < 0 || instruction->additional_index > elem_count) {
                bytecode_validator_report_error(validator, "Index for 'INSERT_ELEMENT' is not in range of the specified aggregate type");
                return false;
            }

            auto dest_elem_type = dest_type->static_array.element_type;

            if (dest_elem_type != instruction->b.type) {
                bytecode_validator_report_error(validator, "The 'b' registers type does noet match the arrays element type ('INSERT_ELEMENT')");
                return false;
            }

            return true;
            break;

        }

        case Bytecode_Opcode::EXTRACT_ELEMENT: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'EXTRACT_ELEMENT' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::STATIC_ARRAY) {
                bytecode_validator_report_error(validator, "The 'a' register for 'EXTRACT_ELEMENT' is not of static array type");
                return false;
            }

            Type *array_type = instruction->a.type;

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register for 'EXTRACT_ELEMENT' must be a temporary");
            }

            if (instruction->b.type != &builtin_type_s64) {
                bytecode_validator_report_error(validator, "The 'b' register for 'EXTRACT_ELEMENT' does not have the expected integer type (s64)");
                return false;
            }

            if (!(instruction->b.flags & BC_REGISTER_FLAG_LITERAL)) {
                bytecode_validator_report_error(validator, "The 'b' register for 'EXTRACT_ELEMENT' must be a literal");
                return false;
            }

            auto index = instruction->b.value.integer.s32;

            assert(array_type->kind == Type_Kind::STATIC_ARRAY);
            if (index < 0 || index > array_type->static_array.count) {
                bytecode_validator_report_error(validator, "The index specified in the 'b' register for 'EXTRACT_ELEMENT' is out of range for the specified aggregate type");
                return false;
            }

            Type *result_type = array_type->static_array.element_type;

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'EXTRACT_ELEMENT' must be a temporary");
                return false;
            }

            if (result_type != instruction->dest.type) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'EXTRACT_ELEMENT' does not match the expected aggregate member type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::AGG_OFFSET_POINTER: {
            Type *aggregate_type = nullptr;

            if (instruction->a.kind == Bytecode_Register_Kind::ALLOC ||
                instruction->a.kind == Bytecode_Register_Kind::GLOBAL) {
                aggregate_type = instruction->a.type;

            } else if (instruction->a.kind == Bytecode_Register_Kind::TEMPORARY) {

                if (instruction->a.type->kind != Type_Kind::POINTER) {
                    bytecode_validator_report_error(validator, "The 'a' register of 'AGG_OFFSET_POINTER' must be of pointer type when it is a temporary");
                    return false;
                }

                aggregate_type = instruction->a.type->pointer.base;

            } else {
                bytecode_validator_report_error(validator, "The 'a' register of 'AGG_OFFSET_POINTER' must be a temporary, alloc or global");
                return false;
            }

            assert(aggregate_type);

            if (!(aggregate_type->flags & TYPE_FLAG_AGGREGATE)) {
                bytecode_validator_report_error(validator, "The 'a' register of 'AGG_OFFSET_POINTER' does not contain an aggregate value");
                return false;
            }

            if (aggregate_type->kind != Type_Kind::STRUCTURE) {
                bytecode_validator_report_error(validator, "The 'a' register of AGG_OFFSET_POINTER' must be a struct");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register of 'AGG_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if (instruction->b.type != &builtin_type_s32) {
                bytecode_validator_report_error(validator, "The 'b' register of 'AGG_OFFSET_POINTER' does not have the right integer type (s32)");
                return false;
            }

            if (!(instruction->b.flags & BC_REGISTER_FLAG_LITERAL)) {
                bytecode_validator_report_error(validator, "The 'b' register of 'AGG_OFFSET_POINTER' must be a literal");
                return false;
            }

            auto index = instruction->b.value.integer.s32;
            assert(aggregate_type);

            assert(aggregate_type->kind == Type_Kind::STRUCTURE);
            if (index < 0 || index > aggregate_type->structure.member_types.count) {
                bytecode_validator_report_error(validator, "The index for 'AGG_OFFSET_POINTER' (specified in the 'b' register) is out of bounds for the specified aggregate type");
                return false;
            }
            Type *result_type = aggregate_type->structure.member_types[index];

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'AGG_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if  (instruction->dest.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'AGG_OFFSET_POINTER' must be of pointer type (pointer to the targeted aggregate member)");
                return false;
            }

            if (instruction->dest.type->pointer.base != result_type) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register for 'AGG_OFFSET_POINTER' does not match the type extracted from the aggregate type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::ARR_OFFSET_POINTER: {
            Type *array_type = nullptr;

            if (instruction->a.kind == Bytecode_Register_Kind::ALLOC ||
                instruction->a.kind == Bytecode_Register_Kind::GLOBAL) {

                array_type = instruction->a.type;

            } else if (instruction->a.kind == Bytecode_Register_Kind::TEMPORARY) {

                if (instruction->a.type->kind != Type_Kind::POINTER) {
                    bytecode_validator_report_error(validator, "The 'a' register of 'ARR_OFFSET_POINTER' must be of pointer type when it is a temporary");
                    return false;
                }

                array_type = instruction->a.type->pointer.base;

            } else {
                bytecode_validator_report_error(validator, "The 'a' register of 'ARR_OFFSET_POINTER' must be a temporary, global or an alloc");
                return false;
            }

            assert(array_type);

            if (array_type->kind != Type_Kind::STATIC_ARRAY) {
                bytecode_validator_report_error(validator, "The 'a' register of ARR_OFFSET_POINTER' must be of static array type");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register of 'ARR_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if (instruction->b.type != &builtin_type_s64) {
                bytecode_validator_report_error(validator, "The 'b' register of 'ARR_OFFSET_POINTER' does not have the right integer type (s64)");
                return false;
            }

            auto index = instruction->b.value.integer.s32;
            assert(array_type);

            assert(array_type->kind == Type_Kind::STATIC_ARRAY);
            if (index < 0 || index > array_type->static_array.count) {
                bytecode_validator_report_error(validator, "The index for 'ARR_OFFSET_POINTER' (specified in the 'b' register) is out of bounds for the specified array type");
                return false;
            }
            Type *result_type = array_type->static_array.element_type;

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ARR_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if  (instruction->dest.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'ARR_OFFSET_POINTER' must be of pointer type (pointer to the array element type)");
                return false;
            }

            if (instruction->dest.type->pointer.base != result_type) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register for 'ARR_OFFSET_POINTER' does not match the type extracted from the array type");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::PTR_OFFSET_POINTER: {

            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register of 'PTR_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'a' register of 'PTR_OFFSET_POINTER' must be a pointer");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'b' register of 'PTR_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if (instruction->b.type != &builtin_type_s64) {
                bytecode_validator_report_error(validator, "The 'b' register of 'PTR_OFFSET_POINTER' does not have the right integer type (s64)");
                return false;
            }

            Type *result_type = instruction->a.type;

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'PTR_OFFSET_POINTER' must be a temporary");
                return false;
            }

            if  (instruction->dest.type->kind != Type_Kind::POINTER) {
                bytecode_validator_report_error(validator, "The 'dest' register for 'PTR_OFFSET_POINTER' must be of pointer type (pointer to the array element type)");
                return false;
            }

            if (instruction->dest.type != result_type) {
                bytecode_validator_report_error(validator, "The type of the 'dest' register for 'PTR_OFFSET_POINTER' does not match the source pointer type");
                return false;
            }

            return true;
            break;
        }
        case Bytecode_Opcode::JMP: {
            if (instruction->a.kind != Bytecode_Register_Kind::BLOCK) {
                bytecode_validator_report_error(validator, "The 'a' register of 'JMP' must be a block");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::JMP_IF: {
            if (instruction->a.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'a' register of 'JMP_IF' must be a temporary.");
                return false;
            }

            if (instruction->b.kind != Bytecode_Register_Kind::BLOCK) {
                bytecode_validator_report_error(validator, "The 'b' register of 'JMP_IF' must be a block");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::BLOCK) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'JMP_IF' must be a block");
                return false;
            }

            if (instruction->a.type->kind != Type_Kind::BOOLEAN) {
                bytecode_validator_report_error(validator, "The 'a' register of 'JMP_IF' must be of boolean type");
                return false;
            }

            auto then_block_handle = instruction->b.block_handle;
            auto else_block_handle = instruction->dest.block_handle;

            auto &blocks = visitor->current_function->blocks;

            if (!(then_block_handle >= 0 && then_block_handle < blocks.count)) {
                bytecode_validator_report_error(validator, "Invalid then block handle");
                return false;
            }

            if (!(else_block_handle >= 0 && else_block_handle < blocks.count)) {
                bytecode_validator_report_error(validator, "Invalid else block handle");
                return false;
            }

            return true;
            break;
        }

        case Bytecode_Opcode::PHI: {
            if (instruction->a.kind != Bytecode_Register_Kind::PHI_ARGS) {
                bytecode_validator_report_error(validator, "The 'a' register of 'PHI' must be a 'PHI_ARGS'.");
                return false;
            }

            if (instruction->dest.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'dest' register of 'PHI' must be a temporary.");
                return false;
            }

            auto dest_type = instruction->dest.type;

            switch (dest_type->kind) {
                case Type_Kind::INVALID:
                case Type_Kind::VOID: {
                    bytecode_validator_report_error(validator, "Invalid dest type for PHI.");
                    return false;
                }
                default: break;
            }

            assert(instruction->a.phi_args_handle < visitor->current_function->phi_args.count);
            auto phi_args = visitor->current_function->phi_args[instruction->a.phi_args_handle];

            if (phi_args.true_value.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'true' value for PHI must be a temporary");
                return false;
            }


            if (phi_args.false_value.kind != Bytecode_Register_Kind::TEMPORARY) {
                bytecode_validator_report_error(validator, "The 'false' value for PHI must be a temporary");
                return false;
            }

            if (phi_args.true_value.type != dest_type) {
                bytecode_validator_report_error(validator, "The 'true' value type does not match PHI type");
                return false;
            }

            if (phi_args.false_value.type != dest_type) {
                bytecode_validator_report_error(validator, "The 'false' value type does not match PHI type");
                return false;
            }

            auto true_handle = phi_args.true_block_handle;
            auto false_handle = phi_args.false_block_handle;

            auto &blocks = visitor->current_function->blocks;

            if (!(true_handle >= 0 && true_handle < blocks.count)) {
                bytecode_validator_report_error(validator, "Invalid 'true' block handle for PHI");
                return false;
            }

            if (!(false_handle >= 0 && false_handle < blocks.count)) {
                bytecode_validator_report_error(validator, "Invalid 'false' block handle for PHI");
                return false;
            }

            return true;
        }
    }

    return false;
}

Array_Ref<Graph_Node> validator_build_block_graph(Bytecode_Function *func)
{

    Dynamic_Array<Graph_Node> nodes;
    dynamic_array_create<Graph_Node>(temp_allocator_allocator(), &nodes, func->blocks.count + 1);

    for (s64 i = 0; i < func->blocks.count; i++) {
        auto node = Graph_Node { .a = nullptr, .b = nullptr, .block = &func->blocks[i] };
        dynamic_array_append(&nodes, node);
    }

    auto exit_node_ = Graph_Node { .a = nullptr, .b = nullptr, .block = nullptr };
    auto exit_node = dynamic_array_append(&nodes, exit_node_);
    exit_node->returns = true;

    for (s64 i = 0; i < func->blocks.count; i++) {

        auto current_node = &nodes[i];

        auto block = &func->blocks[i];
        if (block->instructions.count > 0) {
            auto last_op = block->instructions[block->instructions.count - 1];

            if (last_op.op == Bytecode_Opcode::RETURN_VOID ||
                last_op.op == Bytecode_Opcode::RETURN) {

                current_node->a = exit_node;
                current_node->returns = true;

            }  else if (last_op.op == Bytecode_Opcode::JMP) {

                current_node->a = &nodes[last_op.a.block_handle];
                if (current_node->a->returns) {
                    current_node->returns = true;
                }

            } else if (last_op.op == Bytecode_Opcode::JMP_IF){

                current_node->a = &nodes[last_op.b.block_handle];
                current_node->b = &nodes[last_op.dest.block_handle];

                if (current_node->a->returns || current_node->b->returns) {
                    current_node->returns = true;
                }

            }
        }
    }

    bool changed = true;
    while (changed) {
        changed = false;

        for (s64 i = nodes.count -1; i >= 0; i--) {
            auto node = &nodes[i];
            if (node->returns) continue;
            if ((node->a && node->a->returns) ||
                (node->b && node->b->returns)) {
                node->returns = true;
                changed = true;
            }
        }
    }

    return nodes;
}

String block_graph_to_dot(Array_Ref<Graph_Node> nodes, Bytecode_Function *func, Allocator *allocator)
{
    String_Builder sb;
    string_builder_create(&sb, allocator);

    string_builder_append(&sb, "digraph g { subgraph  cluster_%.*s {\n", (int)func->name.length, func->name.data);
    string_builder_append(&sb, "    label = \"%.*s\";\n", (int)func->name.length, func->name.data);
    string_builder_append(&sb, "    labelloc = \"t\";\n");
    string_builder_append(&sb, "    node [style=filled];\n");

    for (s64 i = 0; i < nodes.count; i++) {
        auto node = nodes[i];
        if (node.block) {

            string_builder_append(&sb, "    %.*s [color=%s];\n",
                   (int)node.block->name.length, node.block->name.data,
                   node.returns ? "green" : "red");

            if (node.a && node.a->block) {
                string_builder_append(&sb, "    %.*s -> %.*s;\n",
                       (int)node.block->name.length, node.block->name.data,
                       (int)node.a->block->name.length, node.a->block->name.data);
            }

            if (node.b && node.b->block) {
                string_builder_append(&sb, "    %.*s -> %.*s;\n",
                       (int)node.block->name.length, node.block->name.data,
                       (int)node.b->block->name.length, node.b->block->name.data);
            }

            if (node.a && !node.a->block) {
                string_builder_append(&sb, "    %.*s -> exit;\n", (int)node.block->name.length, node.block->name.data);
            }

        } else {
            assert(node.returns);
            string_builder_append(&sb, "    exit [color=green];\n");
        }
    }

    string_builder_append(&sb, "}}\n");

    auto result = string_builder_to_string(&sb);
    string_builder_destroy(&sb);
    return result;
}

}}
