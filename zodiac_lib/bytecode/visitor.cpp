#include "bytecode/visitor.h"

#include "defines.h"
#include "type.h"
#include "util/asserts.h"

#include <stdio.h>

namespace Zodiac { namespace Bytecode {

void bytecode_visitor_init(Allocator *allocator, Bytecode_Visitor *visitor, void *user_data, const Array_Ref<Bytecode_Function> &functions, Hash_Table<Bytecode_Instruction_Handle, Source_Pos> *instruction_locations)
{
    *visitor = {};

    visitor->allocator = allocator;
    visitor->user_data = user_data;

    visitor->functions = functions;

    visitor->current_function = nullptr;
    visitor->current_pos = { .fn_index = 0, .block_index = 0, .instruction_index = 0 };

    visitor->visit_function = nullptr;
    visitor->visit_block = nullptr;
    visitor->visit_instruction = nullptr;

    stack_init(allocator, &visitor->arg_stack);

    visitor->instruction_locations = instruction_locations;
}

void bytecode_visitor_free(Bytecode_Visitor *visitor)
{
    stack_free(&visitor->arg_stack);
}

bool visit_bytecode(Bytecode_Visitor *visitor)
{
    auto pos = &visitor->current_pos;

    bool result = true;

    for (pos->fn_index = 0; pos->fn_index < visitor->functions.count; pos->fn_index++) {

        auto fn_handle = (Bytecode_Function_Handle)pos->fn_index;

        auto fn = &visitor->functions[fn_handle];

        if (fn->flags & BC_FUNCTION_FLAG_VALIDATED) {
            continue;
        }

        bool r = true;
        if (visitor->visit_function) {
            if (!visitor->visit_function(visitor, fn_handle)) {
                r = false;
            }
        }

        if (r) {
            if (!visit_function(visitor, fn_handle)) {
                r = false;
            }
        }

        if (r) {
            fn->flags |= BC_FUNCTION_FLAG_VALIDATED;
        } else {
            result = false;
        }
    }

    return result;
}

bool visit_function(Bytecode_Visitor *visitor, Bytecode_Function_Handle fn_handle)
{
    assert(fn_handle >= 0 && fn_handle < visitor->functions.count);
    auto bc_func = &visitor->functions[fn_handle];

    assert(visitor->current_function == nullptr);
    visitor->current_function = bc_func;

    assert(stack_count(&visitor->arg_stack) == 0);

    bool result = true;

    auto pos = &visitor->current_pos;

    for (pos->block_index = 0; pos->block_index < bc_func->blocks.count; pos->block_index++) {

        if (visitor->visit_block) {
            if (!visitor->visit_block(visitor, &bc_func->blocks[pos->block_index])) {
                result = false;
                break;
            }
        }

        if (!visit_block(visitor, &bc_func->blocks[pos->block_index])) {
            result = false;
            break;
        }
    }

    visitor->current_function = nullptr;

    if (result) {
        assert(stack_count(&visitor->arg_stack) == 0);
    }

    return result;
}

bool visit_block(Bytecode_Visitor *visitor, Bytecode_Block *block)
{
    if (visitor->visit_instruction) {

        auto pos = &visitor->current_pos;

        for (pos->instruction_index = 0; pos->instruction_index < block->instructions.count; pos->instruction_index++) {


            Bytecode_Instruction *inst = &block->instructions[pos->instruction_index];

            bool result = visitor->visit_instruction(visitor, inst);

            visit_instruction_post(visitor, inst);

            if (!result)
                return false;
        }

    }

    return true;
}

void visit_instruction_post(Bytecode_Visitor *visitor, Bytecode_Instruction *instruction)
{
    auto op = instruction->op;

    if (op == Bytecode_Opcode::CALL ||
        op == Bytecode_Opcode::CALL_FOREIGN ||
        op == Bytecode_Opcode::CALL_PTR) {

        Type *fn_type = nullptr;

        if (op == Bytecode_Opcode::CALL ||
            op == Bytecode_Opcode::CALL_FOREIGN) {

            assert(instruction->a.kind == Bytecode_Register_Kind::FUNCTION);
            auto fn_handle = instruction->a.value.function_handle;
            assert(fn_handle >= 0 && fn_handle < visitor->functions.count);
            auto fn = &visitor->functions[fn_handle];
            fn_type = fn->type;

        } else {
            assert(op == Bytecode_Opcode::CALL_PTR);

            assert(instruction->a.type->kind == Type_Kind::POINTER);
            assert(instruction->a.type->pointer.base->kind == Type_Kind::FUNCTION);

            fn_type = instruction->a.type->pointer.base;
        }

        assert(fn_type);
        assert(fn_type->kind == Type_Kind::FUNCTION);

        auto arg_count = fn_type->function.parameter_types.count;

        if (stack_count(&visitor->arg_stack) < arg_count) {
            fprintf(stderr, "WARNING: mismatching arg count detected in visit_instruction_post\n");
        }
        auto args_to_pop = (s64)arg_count;
        stack_pop(&visitor->arg_stack, args_to_pop);

    } else if (op == Bytecode_Opcode::PUSH_ARG) {

        assert(instruction->a.kind == Bytecode_Register_Kind::TEMPORARY);
        stack_push(&visitor->arg_stack, instruction->a);

    }
}
}}
