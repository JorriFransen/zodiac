#include "bytecode/printer.h"

#include "zodiac_context.h"
#include "type.h"

namespace Zodiac { namespace Bytecode {

void bytecode_print(Bytecode_Builder *builder, Allocator *allocator)
{
    String_Builder sb;
    string_builder_create(&sb, allocator);

    bytecode_print(builder, &sb);

    auto str = string_builder_to_string(&sb);
    printf("%s\n", str.data);

    free(allocator, str.data);
    string_builder_destroy(&sb);
}

void bytecode_print(Bytecode_Builder *builder, String_Builder *sb)
{
    // TODO: Non builtin types
    // auto &tt = builder->zodiac_context->type_table;

    // for (int64_t i = 0; i < tt.count; i++) {
    //     auto type = tt[i];
    //     if (type->kind == Type_Kind::STRUCTURE) {
    //         assert(type->atom.length);
    //         string_builder_append(sb, "%%%.*s = ", (int)type->atom.length, type->atom.data);
    //         type_to_string(type, sb);
    //         string_builder_append(sb, "\n");
    //     }
    // }
    // if (tt.count) string_builder_append(sb, "\n");

    for (int64_t i = 0; i < builder->globals.count; i++ ) {
        auto glob = &builder->globals[i];
        bytecode_print_global(builder, nullptr, glob, sb);
        string_builder_append(sb, "\n");
    }

    if (builder->globals.count) string_builder_append(sb, "\n");

    for (int64_t i = 0; i < builder->functions.count; i++) {
        auto fn = &builder->functions[i];
        bytecode_print_function(builder, fn, sb);
        string_builder_append(sb, "\n");
    }
}

void bytecode_print_global(Bytecode_Builder *builder, Bytecode_Function *fn, Bytecode_Global *glob, String_Builder *sb)
{
    string_builder_append(sb, "%.*s : ", (int)glob->atom.length, glob->atom.data);

    type_to_string(glob->type, sb);

    if (glob->initial_value.kind != Bytecode_Register_Kind::INVALID) {

        if (glob->constant) {
            string_builder_append(sb, " : ");
        } else {
            string_builder_append(sb, " = ");
        }

        bytecode_print_register(builder, fn, glob->initial_value, sb);
    }

    string_builder_append(sb, ";");
}

void bytecode_print_function(Bytecode_Builder *builder, Bytecode_Function *function, String_Builder *sb)
{
    string_builder_append(sb, "%.*s(", (int)function->name.length, function->name.data);

    auto fn_type = function->type;

    for (s64 i = 0; i < fn_type->function.parameter_types.count; i++) {
        if (i > 0) string_builder_append(sb, ", ");
        string_builder_append(sb, "%%%ll", i);
    }

    string_builder_append(sb, ") -> ");
    auto return_type = fn_type->function.return_type;
    if (return_type->flags & TYPE_FLAG_AGGREGATE) {
        assert(return_type->kind == Type_Kind::STRUCTURE);
        assert(false);
        // string_builder_append(sb, "%.*s", (int)return_type->atom.length, return_type->atom.data);
    } else {
        type_to_string(return_type, sb);
    }

    if (function->flags & BC_FUNCTION_FLAG_NORETURN) {
        string_builder_append(sb, " #noreturn");
    }

    string_builder_append(sb, "\n");

    for (int64_t i = 0; i < function->blocks.count; i++) {
        bytecode_print_block(builder, function, &function->blocks[i], sb, 2);
    }
}

void bytecode_print_block(Bytecode_Builder *builder, Bytecode_Function *fn, Bytecode_Block *block, String_Builder *sb,
                          int indent/*= 0*/)
{
    assert(fn);

    bytecode_print_indent(indent, sb);
    string_builder_append(sb, "%.*s:\n", (int)block->name.length, block->name.data);
    for (int64_t i = 0; i < block->instructions.count; i++) {
        bytecode_print_instruction(builder, fn, &block->instructions[i], sb, indent + 2);
        string_builder_append(sb, "\n");
    }
}

void bytecode_print_instruction(Bytecode_Builder *builder, Bytecode_Function *fn, Bytecode_Instruction *instruction,
                                String_Builder *sb, int indent/*= 0*/)
{
    assert(fn);
    bytecode_print_indent(indent, sb);

    if (instruction->dest.index != -1) {
        assert(instruction->dest.kind == Bytecode_Register_Kind::TEMPORARY ||
               ((instruction->dest.kind == Bytecode_Register_Kind::ALLOC) && instruction->op == Bytecode_Opcode::ALLOC));
        bytecode_print_register(builder, fn, instruction->dest, sb);
        string_builder_append(sb, " = ");
    }

    bool print_a = (instruction->a.index >= 0 ||
                    instruction->a.flags & BC_REGISTER_FLAG_LITERAL ||
                    instruction->a.kind == Bytecode_Register_Kind::TYPE);

    bool print_b = instruction->b.index >= 0 ||
                   instruction->b.flags & BC_REGISTER_FLAG_LITERAL;

    if (instruction->op == Bytecode_Opcode::ALLOC) {
        print_a = false;
        print_b = false;
    }

    switch (instruction->op) {

#define PRINT_OP_TYPED_(op, type) \
    string_builder_append(sb, #op "."); \
    assert(type->kind != Type_Kind::STRUCTURE); \
    type_to_string(type, sb);

#define PRINT_OP_(op) PRINT_OP_TYPED_(op, (instruction->dest.type ?  instruction->dest.type : instruction->a.type))

#define OP_CASE_(op) case Bytecode_Opcode::op: { \
    string_builder_append(sb, #op); \
    break;\
}

#define BINOP_CASE_(op) case Bytecode_Opcode::op: { \
    PRINT_OP_(op) \
    break; \
}


#define UNOP_CASE_TYPED_(op, type) case Bytecode_Opcode::op: { \
    PRINT_OP_TYPED_(op, type) \
    break; \
}

#define UNOP_CASE_(op) UNOP_CASE_TYPED_(op, (instruction->dest.type ? instruction->dest.type : instruction->a.type))

        OP_CASE_(NOP)

        BINOP_CASE_(I_ADD)
        BINOP_CASE_(I_SUB)
        BINOP_CASE_(I_MUL)
        BINOP_CASE_(I_DIV)

        BINOP_CASE_(I_EQ)
        BINOP_CASE_(I_NEQ)
        BINOP_CASE_(I_GT)
        BINOP_CASE_(I_LT)
        BINOP_CASE_(I_GT_EQ)
        BINOP_CASE_(I_LT_EQ)

        BINOP_CASE_(F_ADD)
        BINOP_CASE_(F_SUB)
        BINOP_CASE_(F_MUL)
        BINOP_CASE_(F_DIV)

        BINOP_CASE_(F_EQ)
        BINOP_CASE_(F_NEQ)
        BINOP_CASE_(F_GT)
        BINOP_CASE_(F_LT)
        BINOP_CASE_(F_GT_EQ)
        BINOP_CASE_(F_LT_EQ)

        UNOP_CASE_(SQRT)

        UNOP_CASE_(TRUNC)
        UNOP_CASE_(PRINT)

        UNOP_CASE_(PUSH_ARG)

        case Bytecode_Opcode::CALL: {
            print_a = false;
            string_builder_append(sb, "CALL.");
            auto callee_fn_reg = instruction->a;
            assert(callee_fn_reg.kind == Bytecode_Register_Kind::FUNCTION);
            auto callee_fn = &builder->functions[callee_fn_reg.value.function_handle];
            auto return_type = callee_fn->type->function.return_type;

            if (return_type->flags & TYPE_FLAG_AGGREGATE) {
                assert(false);
                // assert(return_type->atom.length);
                // string_builder_append(sb, "%.*s", (int)return_type->atom.length, return_type->atom.data);
            } else {
                type_to_string(return_type, sb);
            }

            string_builder_append(sb, " @%.*s", (int)callee_fn->name.length, callee_fn->name.data);
            break;
        }

        UNOP_CASE_TYPED_(CALL_FOREIGN, instruction->a.type->function.return_type)

        // UNOP_CASE_TYPED_(CALL_PTR, instruction->a.type->pointer.base->function.return_type)
        case Bytecode_Opcode::CALL_PTR: { assert(false); break; }

        OP_CASE_(RETURN_VOID)
        UNOP_CASE_(RETURN)

        UNOP_CASE_(ALLOC)
        UNOP_CASE_(ADDROF_ALLOC)
        UNOP_CASE_(ADDROF_FUNC)

        UNOP_CASE_(STORE_G)
        UNOP_CASE_(LOAD_G)
        UNOP_CASE_(STORE_A)
        UNOP_CASE_(LOAD_A)
        UNOP_CASE_(STORE_PTR)
        UNOP_CASE_(LOAD_PTR)

        case Bytecode_Opcode::INSERT_VALUE: {
            PRINT_OP_(INSERT_VALUE);
            print_a = false;
            print_b = false;

            if (instruction->a.kind == Bytecode_Register_Kind::INVALID) {
                string_builder_append(sb, " <undef>");
            } else  {
                string_builder_append(sb, " ");
                bytecode_print_register(builder, fn, instruction->a, sb);
            }

            string_builder_append(sb, " ");
            bytecode_print_register(builder, fn, instruction->b, sb);

            string_builder_append(sb, " %ll", instruction->additional_index);
            break;
        }

        UNOP_CASE_(EXTRACT_VALUE)

        case Bytecode_Opcode::INSERT_ELEMENT: {
            PRINT_OP_(INSERT_ELEMENT);
            print_a = false;
            print_b = false;

            if (instruction->a.kind == Bytecode_Register_Kind::INVALID) {
                string_builder_append(sb, " <undef>");
            } else  {
                string_builder_append(sb, " ");
                bytecode_print_register(builder, fn, instruction->a, sb);
            }

            string_builder_append(sb, " ");
            bytecode_print_register(builder, fn, instruction->b, sb);

            string_builder_append(sb, " %ll", instruction->additional_index);
            break;
        }

        UNOP_CASE_(EXTRACT_ELEMENT)

        BINOP_CASE_(AGG_OFFSET_POINTER)
        BINOP_CASE_(ARR_OFFSET_POINTER)

        case Bytecode_Opcode::JMP: {
            assert(instruction->a.kind == Bytecode_Register_Kind::BLOCK);
            Bytecode_Block_Handle block_handle = instruction->a.block_handle;
            assert(block_handle >= 0 && block_handle < fn->blocks.count);
            Bytecode_Block *block = &fn->blocks[block_handle];
            string_builder_append(sb, "JMP %.*s", (int)block->name.length, block->name.data);
            print_a = false;
            print_b = false;
            break;
        }

        case Bytecode_Opcode::JMP_IF: {
            assert(instruction->a.kind == Bytecode_Register_Kind::TEMPORARY);
            assert(instruction->a.type->kind == Type_Kind::BOOLEAN);

            assert(instruction->b.kind == Bytecode_Register_Kind::BLOCK);
            auto then_block_handle = instruction->b.block_handle;
            assert(then_block_handle >= 0 && then_block_handle < fn->blocks.count);
            auto then_block_name = fn->blocks[then_block_handle].name;

            assert(instruction->dest.kind == Bytecode_Register_Kind::BLOCK);
            auto else_block_handle = instruction->dest.block_handle;
            assert(else_block_handle >= 0 && else_block_handle < fn->blocks.count);
            auto else_block_name = fn->blocks[else_block_handle].name;

            string_builder_append(sb, "JMP_IF ");
            bytecode_print_register(builder, fn, instruction->a, sb);

            string_builder_append(sb, " ? %.*s : %.*s",
                                   (int)then_block_name.length, then_block_name.data,
                                   (int)else_block_name.length, else_block_name.data);
            print_a = false;
            print_b = false;
            break;
        }

        case Bytecode_Opcode::PHI: {
            PRINT_OP_(PHI);
            string_builder_append(sb, " ");
            bytecode_print_register(builder, fn, instruction->a, sb);
            break;
        }

#undef PRINT_OP_TYPED_
#undef PRINT_OP_
#undef OP_CASE_
#undef BINOP_CASE_
#undef UNOP_CASE_

    }

    if (print_a) {
        string_builder_append(sb, " ");
        bytecode_print_register(builder, fn, instruction->a, sb);
    }
    if (print_a && print_b) {
        string_builder_append(sb, ",");
    }
    if (print_b) {
        string_builder_append(sb, " ");
        bytecode_print_register(builder, fn, instruction->b, sb);
    }
}

void bytecode_print_register(Bytecode_Builder *builder, Bytecode_Function *fn, const Bytecode_Register &reg, String_Builder *sb)
{
    switch (reg.kind) {
        case Bytecode_Register_Kind::INVALID: assert(false); break;

        case Bytecode_Register_Kind::TEMPORARY: {

            if (reg.flags & BC_REGISTER_FLAG_LITERAL) {
                assert(reg.kind == Bytecode_Register_Kind::TEMPORARY);

                if(reg.type->kind == Type_Kind::INTEGER) {

                    auto ri = reg.value.integer;
                    if (reg.type->integer.sign) {
                        switch (reg.type->bit_size) {
                            default: assert(false); break;
                            case 8: string_builder_append(sb, "%d", ri.s8); break;
                            case 16: string_builder_append(sb, "%d", ri.s16); break;
                            case 32: string_builder_append(sb, "%d", ri.s32); break;
                            case 64: string_builder_append(sb, "%d", ri.s64); break;
                        }
                    } else {
                        switch (reg.type->bit_size) {
                            default: assert(false); break;
                            case 8: string_builder_append(sb, "%u", ri.u8); break;
                            case 16: string_builder_append(sb, "%u", ri.u16); break;
                            case 32: string_builder_append(sb, "%u", ri.u32); break;
                            case 64: string_builder_append(sb, "%u", ri.u64); break;
                        }
                    }

                } else if (reg.type->kind == Type_Kind::FLOAT) {

                    auto rr = reg.value.real;
                    if (reg.type->bit_size == 32) {
                        string_builder_append(sb, "%f", rr.r32);
                    } else if (reg.type->bit_size == 64) {
                        string_builder_append(sb, "%f", rr.r64);
                    } else {
                        assert(false && !"Unsupported literal float size register for printing");
                    }

                } else if (reg.type->kind == Type_Kind::BOOLEAN) {
                    if (reg.value.boolean) {
                        string_builder_append(sb, "TRUE");
                    } else {
                        string_builder_append(sb, "FALSE");
                    }
                } else if (reg.type->kind == Type_Kind::POINTER) {


                    string_builder_append(sb, "%p", reg.value.pointer);

                } else {
                    assert(false && !"Unsupported literal type register for printing");
                }
            } else {
                string_builder_append(sb, "%%%d", reg.index);
            }
            break;
        }

        case Bytecode_Register_Kind::FUNCTION: {
            auto fn_handle = reg.value.function_handle;
            assert(fn_handle >= 0 && fn_handle <= builder->functions.count);
            auto fn = builder->functions[fn_handle];
            string_builder_append(sb, "%s", fn.name);
            break;
        }

        case Bytecode_Register_Kind::BLOCK: assert(false); break;

        case Bytecode_Register_Kind::ALLOC: {
            if (reg.alloc_name) {
                string_builder_append(sb, "%%%s", reg.alloc_name);
            } else {
                string_builder_append(sb, "%%%d", reg.index);
            }
            break;
        }

        case Bytecode_Register_Kind::TYPE: {
            type_to_string(reg.type, sb);
            break;
        }

        case Bytecode_Register_Kind::GLOBAL: {
            auto index = reg.index;
            assert(index >= 0 && index < builder->globals.count);
            auto glob = &builder->globals[index];

            string_builder_append(sb, "@%.*s", (int)glob->atom.length, glob->atom.data);
            break;
        }

        case Bytecode_Register_Kind::PHI_ARGS: {
            auto index = reg.phi_args_handle;
            assert(index >= 0 && index < fn->phi_args.count);

            auto phi_args = fn->phi_args[index];

            assert(phi_args.true_block_handle > 0 && phi_args.true_block_handle < fn->blocks.count);
            auto true_block = fn->blocks[phi_args.true_block_handle];

            assert(phi_args.false_block_handle > 0 && phi_args.false_block_handle < fn->blocks.count);
            auto false_block = fn->blocks[phi_args.false_block_handle];

            bytecode_print_register(builder, fn, phi_args.true_value, sb);
            string_builder_append(sb, " %.*s, ",
                                   (int)true_block.name.length, true_block.name.data);
            bytecode_print_register(builder, fn, phi_args.false_value, sb);
            string_builder_append(sb, " %.*s",
                                   (int)false_block.name.length, false_block.name.data);

            break;
        }

        case Bytecode_Register_Kind::UNDEF: {
            string_builder_append(sb, "<undef>");
            break;
        }
    }
}

void bytecode_print_indent(int indent, String_Builder *sb)
{
    assert(indent >= 0 && indent <= 20);
    const char *spaces = "                    ";

    string_builder_append(sb, "%.*s", indent, spaces);
}
}}