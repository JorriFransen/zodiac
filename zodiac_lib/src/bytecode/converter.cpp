#include "bytecode/converter.h"

#include "ast.h"
#include "atom.h"
#include "common.h"
#include "constant_resolver.h"
#include "containers/dynamic_array.h"
#include "error.h"
#include "memory/allocator.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

namespace Zodiac { namespace Bytecode {

Bytecode_Converter bytecode_converter_create(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb)
{
    Bytecode_Converter result = {};

    result.allocator = allocator;
    result.context = context;
    result.builder = bb;

    hash_table_create(allocator, &result.functions);
    hash_table_create(allocator, &result.allocations);
    hash_table_create(allocator, &result.globals);
    hash_table_create(allocator, &result.run_directives);

    result.run_directive_count = 0;

    return result;
}

void bytecode_converter_destroy(Bytecode_Converter *bc)
{
    hash_table_free(&bc->functions);
    hash_table_free(&bc->allocations);
    hash_table_free(&bc->globals);
    hash_table_free(&bc->run_directives);
}

void emit_bytecode(Resolver *resolver, Bytecode_Converter *bc)
{
    assert(resolver);
    assert(bc);

    for (s64 i = 0; i < resolver->nodes_to_emit_bytecode.count; i++) {

        Flat_Root_Node *root_node = resolver->nodes_to_emit_bytecode[i];

        switch (root_node->root.kind) {

            case Flat_Node_Kind::DECL: {
                ast_decl_to_bytecode(bc, root_node->root.decl);
                break;
            }

            case Flat_Node_Kind::STMT: assert(false); break;
            case Flat_Node_Kind::EXPR: assert(false); break;
            case Flat_Node_Kind::TYPE_SPEC: assert(false); break;
            case Flat_Node_Kind::PARAM_DECL: assert(false); break;
            case Flat_Node_Kind::FIELD_DECL: assert(false); break;

            case Flat_Node_Kind::FUNCTION_PROTO: break;
        }

        dynamic_array_remove_ordered(&resolver->nodes_to_emit_bytecode, i);
        i -= 1;

        if (root_node->root.kind == Flat_Node_Kind::DECL &&
            root_node->root.decl->kind == AST_Declaration_Kind::RUN_DIRECTIVE) {
            dynamic_array_append(&resolver->nodes_to_run_bytecode, root_node);
        }
    }
}

void ast_decl_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl)
{
    assert(bc);
    assert(decl);
    assert(decl->kind != AST_Declaration_Kind::INVALID);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (DECL_IS_GLOBAL(decl)) {

                Type *global_type = decl->variable.resolved_type;
                assert(global_type);
                auto global_name = decl->identifier.name;

                Bytecode_Register initial_value_reg = {};
                if (decl->variable.value) {
                    initial_value_reg = ast_expr_to_bytecode(bc, decl->variable.value);
                }

                auto global_handle = bytecode_create_global(bc->builder, global_name, global_type, initial_value_reg);

                hash_table_add(&bc->globals, decl, global_handle);

            } else {
                if (decl->variable.value) {

                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, decl, &alloc_reg);
                    assert(found)

                    if (!EXPR_IS_CONST(decl->variable.value) || decl->variable.value->kind == AST_Expression_Kind::RUN_DIRECTIVE) { // Constant should have been emitted when registering the allocation
                        Bytecode_Register value_reg = ast_expr_to_bytecode(bc, decl->variable.value);
                        bytecode_emit_store_alloc(bc->builder, value_reg, alloc_reg);
                    }
                }
            }
            break;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {

            if (decl->variable.value->kind == AST_Expression_Kind::RUN_DIRECTIVE) {
                // This will ensure then generated_expression on the directive is set
                //   to the expression generated by the run directive. We don't need
                //   the result register because constants are emitted as literals
                //   via the constant resolver.
                ast_expr_to_bytecode(bc, decl->variable.value);
                break;
            }

            if (decl->variable.resolved_type->kind == Type_Kind::INTEGER ||
                decl->variable.resolved_type == &builtin_type_unsized_integer) {
                // No need to emit anything
                break;
            } else {
                assert_msg(false, "Not implemented")
            }

            assert(false);
            break;
        }

        case AST_Declaration_Kind::PARAMETER: assert(false); break;
        case AST_Declaration_Kind::FIELD: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {
            ast_function_to_bytecode(bc, decl);
            break;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION:
            // leaf
            break;

        case AST_Declaration_Kind::RUN_DIRECTIVE: {

            auto directive = decl->directive;
            Bytecode_Function_Handle fn_handle = create_run_wrapper(bc, directive);

            debug_assert(!hash_table_find(&bc->run_directives, directive));
            hash_table_add(&bc->run_directives, directive, fn_handle);

            break;
        }
    }
}

void ast_function_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl)
{
    assert(bc);
    assert(decl);
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    assert(decl->function.type);
    assert(decl->function.type->kind == Type_Kind::FUNCTION);
    assert(decl->identifier.name.data);

    Bytecode_Function_Handle fn_handle = bytecode_function_create(bc->builder, decl->identifier.name, decl->function.type);
    hash_table_add(&bc->functions, decl, fn_handle);

    if (decl->function.variables.count) {

        Bytecode_Block_Handle allocs_block_handle = bytecode_append_block(bc->builder, fn_handle, "allocs");
        bytecode_set_insert_point(bc->builder, fn_handle, allocs_block_handle);

        bool has_inits = false;

        for (s64 i = 0; i < decl->function.variables.count; i++) {
            AST_Declaration *var_decl = decl->function.variables[i];
            assert(var_decl->variable.resolved_type);

            Type *alloc_type = var_decl->variable.resolved_type;
            auto alloc_name = var_decl->identifier.name;
            auto alloc_reg = bytecode_emit_alloc(bc->builder, alloc_type, alloc_name.data);

            debug_assert(!hash_table_find(&bc->allocations, var_decl));

            hash_table_add(&bc->allocations, var_decl, alloc_reg);

            if (var_decl->variable.value && EXPR_IS_CONST(var_decl->variable.value)) has_inits = true;
        }

        if (has_inits) {
            Bytecode_Block_Handle inits_block_handle = bytecode_append_block(bc->builder, fn_handle, "inits");
            bytecode_emit_jmp(bc->builder, inits_block_handle);

            bytecode_set_insert_point(bc->builder, fn_handle, inits_block_handle);

            for (s64 i = 0; i < decl->function.variables.count; i++) {
                AST_Declaration *var_decl = decl->function.variables[i];
                if (var_decl->variable.value && EXPR_IS_CONST(var_decl->variable.value) && var_decl->variable.value->kind != AST_Expression_Kind::RUN_DIRECTIVE) {
                    Bytecode_Register value = ast_expr_to_bytecode(bc, var_decl->variable.value);

                    // TODO: Temporarily store this on the stack from before?
                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, var_decl, &alloc_reg);
                    assert(found)

                    bytecode_emit_store_alloc(bc->builder, value, alloc_reg);
                }
            }
        }
        Bytecode_Block_Handle start_block_handle = bytecode_append_block(bc->builder, fn_handle, "start");
        bytecode_emit_jmp(bc->builder, start_block_handle);
        bytecode_set_insert_point(bc->builder, fn_handle, start_block_handle);
    } else {
        Bytecode_Block_Handle entry_block_handle = bytecode_append_block(bc->builder, fn_handle, "entry");
        bytecode_set_insert_point(bc->builder, fn_handle, entry_block_handle);
    }

    for (s64 i = 0; i < decl->function.body.count; i++) {

        AST_Statement *stmt = decl->function.body[i];

        Bytecode_Block *current_block = bytecode_get_insert_block(bc->builder);
        if (bytecode_block_is_terminated(current_block)) {
            zodiac_report_error(bc->context, Zodiac_Error_Kind::ZODIAC_BC_CONVERSION_ERROR, stmt, "Unreachable code detected");
            return;
        }

        ast_stmt_to_bytecode(bc, stmt);
    }

    auto fn = &bc->builder->functions[fn_handle];
    auto block = &fn->blocks[bc->builder->insert_block_index];

    if (!block->instructions.count) {
        if (fn->type->function.return_type->kind == Type_Kind::VOID) {
            bytecode_emit_return(bc->builder);
        } else {
            // We might sometimes be left with an empty block at the end, remove it if this is the case.
            assert(fn->blocks.count >= 2);
            fn->blocks.count -= 1;
            dynamic_array_free(&block->instructions);
        }
    }

    if (fn->type->function.return_type->kind == Type_Kind::VOID) {
        bool emit_void_return = false;
        if (block->instructions.count) {
            auto last_op = block->instructions[block->instructions.count - 1];

            if (last_op.op != Bytecode_Opcode::RETURN &&
                last_op.op != Bytecode_Opcode::RETURN_VOID) {

                emit_void_return = true;
            }
        } else {
            emit_void_return = true;
        }

        if (emit_void_return) {
            bytecode_emit_return(bc->builder);
        }
    }
}

void ast_stmt_to_bytecode(Bytecode_Converter *bc, AST_Statement *stmt)
{
    assert(bc);
    assert(stmt);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::BLOCK: {
            for (s64 i = 0; i < stmt->block.statements.count; i++) {

                Bytecode_Block *current_block = bytecode_get_insert_block(bc->builder);
                if (bytecode_block_is_terminated(current_block)) {
                    zodiac_report_error(bc->context, Zodiac_Error_Kind::ZODIAC_BC_CONVERSION_ERROR, stmt, "Unreachable code detected");
                    return;
                }

                ast_stmt_to_bytecode(bc, stmt->block.statements[i]);
            }
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_bytecode(bc, stmt->decl.decl);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            Bytecode_Register lvalue_reg = ast_lvalue_to_bytecode(bc, stmt->assign.dest);
            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, stmt->assign.value);

            if (lvalue_reg.kind == Bytecode_Register_Kind::ALLOC) {
                bytecode_emit_store_alloc(bc->builder, value_reg, lvalue_reg);
            } else if (lvalue_reg.kind == Bytecode_Register_Kind::GLOBAL) {
                bytecode_emit_store_global(bc->builder, value_reg, lvalue_reg.index);
            } else if (lvalue_reg.kind == Bytecode_Register_Kind::TEMPORARY) {
                assert(lvalue_reg.type->kind == Type_Kind::POINTER);
                assert(lvalue_reg.type->pointer.base == value_reg.type);
                bytecode_emit_store_pointer(bc->builder, value_reg, lvalue_reg);
            } else {
                assert(false);
            }

            break;
        }

        case AST_Statement_Kind::CALL: {
            ast_expr_to_bytecode(bc, stmt->call.call);
            break;
        }

        case AST_Statement_Kind::IF: {
            auto cfn = (Bytecode_Function_Handle)bc->builder->insert_fn_index;
            auto ta = &bc->context->temp_allocator;

            auto temp_blocks = temp_array_create<Bytecode_Block_Handle>(ta);

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                if (i > 0) {
                    auto elif_handle = bytecode_append_block(bc->builder, cfn, "elif_cond");
                    dynamic_array_append(&temp_blocks.array, elif_handle);
                }
                auto block_handle = bytecode_append_block(bc->builder, cfn, "then");
                dynamic_array_append(&temp_blocks.array, block_handle);
            }

            if (stmt->if_stmt.else_stmt) {
                auto block_handle = bytecode_append_block(bc->builder, cfn, "else");
                dynamic_array_append(&temp_blocks.array, block_handle);
            }

            auto post_if_block_handle = bytecode_append_block(bc->builder, cfn, "post_if");
            dynamic_array_append(&temp_blocks.array, post_if_block_handle);

            auto block_index = 0;
            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = &stmt->if_stmt.blocks[i];

                if (i > 0) {
                    // The first condition doesn't need it's own block
                    bytecode_set_insert_point(bc->builder, cfn, temp_blocks.array[block_index++]);
                }
                Bytecode_Register cond_reg = ast_expr_to_bytecode(bc, if_block->cond);

                auto then_block = temp_blocks.array[block_index++];
                auto else_block = temp_blocks.array[block_index];

                bytecode_emit_jmp_if(bc->builder, cond_reg, then_block, else_block);

                bytecode_set_insert_point(bc->builder, cfn, then_block);
                ast_stmt_to_bytecode(bc, if_block->then);
                bytecode_set_insert_point(bc->builder, cfn, then_block);

                if (!bytecode_block_is_terminated(bc->builder, cfn, then_block)) {
                    bytecode_emit_jmp(bc->builder, post_if_block_handle);
                }

            }

            if (stmt->if_stmt.else_stmt) {

                assert(temp_blocks.array.count >= 2);
                auto else_block = temp_blocks.array[temp_blocks.array.count - 2];

                bytecode_set_insert_point(bc->builder, cfn, else_block);
                ast_stmt_to_bytecode(bc, stmt->if_stmt.else_stmt);
                bytecode_set_insert_point(bc->builder, cfn, else_block);

                if (!bytecode_block_is_terminated(bc->builder, cfn, else_block)) {
                    bytecode_emit_jmp(bc->builder, post_if_block_handle);
                }
            }

            bytecode_set_insert_point(bc->builder, cfn, post_if_block_handle);

            temp_array_destroy(&temp_blocks);

            break;
        }

        case AST_Statement_Kind::WHILE: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                Bytecode_Register return_value_register = ast_expr_to_bytecode(bc, stmt->return_stmt.value);
                bytecode_emit_return(bc->builder, return_value_register);
            } else {
                bytecode_emit_return(bc->builder);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            for (s64 i = 0; i < stmt->print_expr.expressions.count; i++) {
                auto type = stmt->print_expr.expressions[i]->resolved_type;
                assert(type->kind == Type_Kind::INTEGER ||
                       type->kind == Type_Kind::FLOAT   ||
                       type->kind == Type_Kind::BOOLEAN ||
                       type == &builtin_type_String);
                Bytecode_Register value_reg = ast_expr_to_bytecode(bc, stmt->print_expr.expressions[i]);
                bytecode_emit_print(bc->builder, value_reg);
            }

            auto newline_value_reg = bytecode_string_literal(bc->builder, "\n");
            bytecode_emit_print(bc->builder, newline_value_reg);
            break;
        }
    }
}

Bytecode_Register ast_lvalue_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr)
{
    assert(bc);
    assert(expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(expr->identifier.scope);
            Symbol *ident_sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
            assert(ident_sym);
            assert(ident_sym->decl);
            // assert(ident_sym->decl->kind == AST_Declaration_Kind::VARIABLE);

            switch (ident_sym->decl->kind) {

                case AST_Declaration_Kind::INVALID: assert(false); break;

                case AST_Declaration_Kind::VARIABLE: {
                    if (DECL_IS_GLOBAL(ident_sym->decl)) {

                        Bytecode_Global_Handle glob_handle;
                        bool found = hash_table_find(&bc->globals, ident_sym->decl, &glob_handle);
                        assert(found);

                        debug_assert(glob_handle < bc->builder->globals.count);
                        auto global = bc->builder->globals[glob_handle];

                        Bytecode_Register global_reg;
                        bool reg_found = hash_table_find(&bc->builder->global_registers, global.atom, &global_reg);
                        assert(reg_found);

                        debug_assert(global_reg.kind == Bytecode_Register_Kind::GLOBAL);
                        return global_reg;

                    } else {
                        Bytecode_Register alloc_reg;
                        bool found = hash_table_find(&bc->allocations, ident_sym->decl, &alloc_reg);
                        assert(found);

                        return alloc_reg;
                    }
                }

                case AST_Declaration_Kind::CONSTANT_VARIABLE: assert(false); break;

                case AST_Declaration_Kind::PARAMETER: {
                    assert(false); // We need to store arguments in local variables to make this work.
                    break;
                }

                case AST_Declaration_Kind::FIELD: assert(false); break;
                case AST_Declaration_Kind::FUNCTION: assert(false); break;
                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
                case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
            }
        }

        case AST_Expression_Kind::MEMBER: {
            assert(expr->member.index_in_parent >= 0);
            Bytecode_Register base_reg = ast_lvalue_to_bytecode(bc, expr->member.base);
            return bytecode_emit_aggregate_offset_pointer(bc->builder, base_reg, expr->member.index_in_parent);
        }

        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;
        case AST_Expression_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;

    }

    assert(false);
    return {};
}

Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr)
{
    assert(bc);
    assert(expr);

    if (expr->kind != AST_Expression_Kind::INTEGER_LITERAL &&
        expr->kind != AST_Expression_Kind::REAL_LITERAL &&
        expr->kind != AST_Expression_Kind::STRING_LITERAL &&
        expr->kind != AST_Expression_Kind::RUN_DIRECTIVE && EXPR_IS_CONST(expr)) {
        assert_msg(expr->resolved_type->kind == Type_Kind::INTEGER ||
                   expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER ||
                   expr->resolved_type->kind == Type_Kind::FLOAT ||
                   expr->resolved_type->kind == Type_Kind::BOOLEAN ||
                   expr->resolved_type->kind == Type_Kind::STRUCTURE && expr->kind == AST_Expression_Kind::COMPOUND,
                   "Constant expression substition not supported for this type");

        return ast_const_expr_to_bytecode(bc, expr);
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            Type *literal_type = expr->resolved_type;
            if (literal_type->kind == Type_Kind::UNSIZED_INTEGER) {
                literal_type = &builtin_type_s64;
            }
            assert(literal_type->kind == Type_Kind::INTEGER);
            return bytecode_integer_literal(bc->builder, literal_type, expr->integer_literal.value);
        }

        case AST_Expression_Kind::REAL_LITERAL: {
            Type *literal_type = expr->resolved_type;
            assert(literal_type->kind == Type_Kind::FLOAT);
            return bytecode_real_literal(bc->builder, literal_type, expr->real_literal.value);
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            return bytecode_string_literal(bc->builder, &expr->string_literal.atom);
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {

            assert(expr->identifier.scope);
            Symbol *ident_sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
            assert(ident_sym);
            assert(ident_sym->decl);
            AST_Declaration *ident_decl = ident_sym->decl;

            switch (ident_decl->kind) {
                case AST_Declaration_Kind::INVALID: assert(false); break;

                case AST_Declaration_Kind::VARIABLE: {
                    assert(ident_decl->variable.resolved_type);

                    if (DECL_IS_GLOBAL(ident_decl)) {

                        Bytecode_Global_Handle global_handle;
                        bool found = hash_table_find(&bc->globals, ident_decl, &global_handle);
                        assert(found);

                        return bytecode_emit_load_global(bc->builder, global_handle);

                    } else {
                        Bytecode_Register alloc_reg;
                        bool found = hash_table_find(&bc->allocations, ident_decl, &alloc_reg);
                        assert(found);

                        return bytecode_emit_load_alloc(bc->builder, alloc_reg);
                    }
                }

                case AST_Declaration_Kind::CONSTANT_VARIABLE: {
                    assert(ident_decl->variable.value);
                    return ast_expr_to_bytecode(bc, ident_decl->variable.value);
                }

                case Zodiac::AST_Declaration_Kind::PARAMETER: {
                    assert(ident_decl->parameter.resolved_type);

                    auto func_decl = enclosing_function(expr->identifier.scope);
                    assert(func_decl);
                    assert(func_decl->kind == AST_Declaration_Kind::FUNCTION);

                    s64 param_index = -1;
                    for (s64 i = 0; i < func_decl->function.params.count; i++) {
                        if (func_decl->function.params[i]->identifier.name == expr->identifier.name) {
                            param_index = i;
                            break;
                        }
                    }

                    assert(param_index >= 0);

                    return bytecode_emit_load_argument(bc->builder, param_index);
                }

                case Zodiac::AST_Declaration_Kind::FIELD: assert(false); break;

                case AST_Declaration_Kind::FUNCTION: assert(false); break;
                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
                case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: {
            Bytecode_Register base_reg = ast_lvalue_to_bytecode(bc, expr->member.base);
            Bytecode_Register addr_reg = bytecode_emit_aggregate_offset_pointer(bc->builder, base_reg, expr->member.index_in_parent);
            return bytecode_emit_load_pointer(bc->builder, addr_reg);
        }

        case AST_Expression_Kind::INDEX: assert(false); break;

        case AST_Expression_Kind::CALL: {

            AST_Expression *base = expr->call.base;
            assert(base->kind == AST_Expression_Kind::IDENTIFIER);

            Symbol *sym = scope_get_symbol(base->identifier.scope, base->identifier.name);
            assert(sym && sym->kind == Symbol_Kind::FUNC);
            assert(sym->decl && sym->decl->kind == AST_Declaration_Kind::FUNCTION);


            Bytecode_Function_Handle fn_handle;
            bool found = hash_table_find(&bc->functions, sym->decl, &fn_handle);
            assert(found);

            auto arg_count = expr->call.args.count;

            for (s64 i = 0; i < expr->call.args.count; i++) {
                auto arg_expr = expr->call.args[i];
                Bytecode_Register arg_reg = ast_expr_to_bytecode(bc, arg_expr);
                bytecode_emit_push_arg(bc->builder, arg_reg);
            }

            return bytecode_emit_call(bc->builder, fn_handle, arg_count);
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false); break;

        case AST_Expression_Kind::BINARY: {

            auto lhs = expr->binary.lhs;
            auto rhs = expr->binary.rhs;

            assert(lhs->resolved_type == rhs->resolved_type);

            Bytecode_Register lhs_reg = ast_expr_to_bytecode(bc, lhs);
            Bytecode_Register rhs_reg = ast_expr_to_bytecode(bc, rhs);

            switch (expr->binary.op) {
                case AST_Binary_Operator::INVALID: assert(false); break;

                case AST_Binary_Operator::ADD: {
                    return bytecode_emit_add(bc->builder, lhs_reg, rhs_reg);
                }

                case AST_Binary_Operator::SUB: {
                    return bytecode_emit_sub(bc->builder, lhs_reg, rhs_reg);
                }

                case AST_Binary_Operator::MUL: {
                    return bytecode_emit_mul(bc->builder, lhs_reg, rhs_reg);
                }

                case AST_Binary_Operator::DIV: {
                    return bytecode_emit_div(bc->builder, lhs_reg, rhs_reg);
                }

                case AST_Binary_Operator::EQ: {
                    return bytecode_emit_eq(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::NEQ: assert(false); break;
                case AST_Binary_Operator::LT: assert(false); break;
                case AST_Binary_Operator::GT: assert(false); break;
                case AST_Binary_Operator::LTEQ: assert(false); break;
                case AST_Binary_Operator::GTEQ: assert(false); break;
            }
            break;
        }

        case AST_Expression_Kind::CAST: {
            debug_assert(expr->resolved_type && expr->cast.resolved_type);
            debug_assert(expr->resolved_type == expr->cast.resolved_type);

            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, expr->cast.value);
            return bytecode_emit_cast(bc->builder, expr->cast.resolved_type, value_reg);
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: {

            assert(EXPR_IS_TYPED(expr));
            assert(expr->directive.directive->kind == AST_Directive_Kind::RUN);
            assert(expr->directive.directive->run.kind == AST_Run_Directive_Kind::EXPR);

            auto directive = &expr->directive;

            Bytecode_Function_Handle wrapper_handle = create_run_wrapper(bc, directive->directive);
            Interpreter_Register result = execute_run_wrapper(bc, wrapper_handle);
            assert(result.type);
            assert(result.type == expr->resolved_type);

            Source_Range range = expr->range;

            Scope *scope = directive->directive->run.scope;
            assert(scope);

            AST_Expression *new_expr = interpreter_register_to_ast_expression(bc, result, scope, range);
            assert(new_expr->resolved_type);
            assert(EXPR_IS_CONST(new_expr));

            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, new_expr);
            directive->generated_expression = new_expr;

            return value_reg;
        }

        case Zodiac::AST_Expression_Kind::COMPOUND: assert(false); break;
    }

    assert(false);
    return {};
}

Bytecode_Register ast_const_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr)
{
    assert(bc);
    assert(expr);
    assert(EXPR_IS_CONST(expr));

    auto type = expr->resolved_type;

    switch (type->kind) {

        default: assert_msg(false, "Unsupported type in ast_const_expr_to_bytecode()");

        case Type_Kind::UNSIZED_INTEGER:
        case Type_Kind::INTEGER: {

            Type *literal_type = type;
            if (literal_type->kind == Type_Kind::UNSIZED_INTEGER) {
                literal_type = &builtin_type_s64;
            }

            Integer_Value result_value = resolve_constant_integer_expr(expr, literal_type);
            return bytecode_integer_literal(bc->builder, literal_type, result_value);
        }

        case Type_Kind::BOOLEAN: {
            Type *bool_type = type;
            assert(bool_type->kind == Type_Kind::BOOLEAN);
            return bytecode_boolean_literal(bc->builder, bool_type, expr->bool_literal);
        }

        case Type_Kind::STRUCTURE: {

            Dynamic_Array<Bytecode_Register> members;
            dynamic_array_create<Bytecode_Register>(bc->allocator, &members, expr->compound.expressions.count);

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {
                auto member_expr = expr->compound.expressions[i];
                assert(EXPR_IS_CONST(member_expr));
                Bytecode_Register member_reg = ast_const_expr_to_bytecode(bc, member_expr);
                dynamic_array_append(&members, member_reg);
            }

            return bytecode_aggregate_literal(bc->builder, members, type);
        }
    }

    assert(false);
    return {};
}

Bytecode_Function_Handle create_run_wrapper(Bytecode_Converter *bc, AST_Directive *run_directive)
{
    debug_assert(bc && run_directive);
    debug_assert(run_directive->kind == AST_Directive_Kind::RUN);

    // We might call this function when we are in the middle of emitting another function,
    //   so we cache the current insert point, and reset it before we return
    auto original_insert_fn_index = bc->builder->insert_fn_index;
    auto original_insert_block_index = bc->builder->insert_block_index;

    char buf[256];
    auto len = string_format(buf, "run_wrapper_%i", bc->run_directive_count);
    assert(len < 256);

    bc->run_directive_count += 1;

    Type *return_type = &builtin_type_void;

    bool return_value = false;

    if (run_directive->run.kind == AST_Run_Directive_Kind::EXPR) {
        return_type = run_directive->run.expr->resolved_type;
        if (return_type != &builtin_type_void) return_value = true;
    }

    Type *run_wrapper_type = get_function_type(return_type, {}, &bc->context->ast_allocator);

    Atom run_wrapper_name = atom_get(&bc->context->atoms, buf);
    auto fn_handle = bytecode_function_create(bc->builder, run_wrapper_name, run_wrapper_type, BC_FUNCTION_FLAG_RUN_WRAPPER);
    auto entry_block = bytecode_append_block(bc->builder, fn_handle, "entry");
    bytecode_set_insert_point(bc->builder, fn_handle, entry_block);

    Bytecode_Register result_register;

    switch (run_directive->run.kind) {

        case AST_Run_Directive_Kind::INVALID: assert(false); break;

        case AST_Run_Directive_Kind::EXPR: {
            result_register = ast_expr_to_bytecode(bc, run_directive->run.expr);
            break;
        }

        case AST_Run_Directive_Kind::STMT: {
            auto stmt = run_directive->run.stmt;
            assert(stmt->kind == AST_Statement_Kind::BLOCK ||
                   stmt->kind == AST_Statement_Kind::PRINT);
            ast_stmt_to_bytecode(bc, stmt);
            break;
        }
    }

    if (return_value) {
        bytecode_emit_return(bc->builder, result_register);
    } else {
        bytecode_emit_return(bc->builder);
    }

    bc->builder->insert_fn_index = original_insert_fn_index;
    bc->builder->insert_block_index = original_insert_block_index;

    return fn_handle;
}

Interpreter_Register execute_run_wrapper(Bytecode_Converter *bc, Bytecode_Function_Handle fn_handle)
{
    File_Handle stdout_file;
    filesystem_stdout_file(&stdout_file);
    return execute_run_wrapper(bc, fn_handle, stdout_file);
}

Interpreter_Register execute_run_wrapper(Bytecode_Converter *bc, Bytecode_Function_Handle fn_handle, File_Handle stdout_file)
{
    debug_assert(bc && fn_handle >= 0 && stdout_file.valid);

#ifndef NDEBUG
    debug_assert(fn_handle >= 0 && fn_handle < bc->builder->functions.count);
    auto fn = &bc->builder->functions[fn_handle];
    ZTRACE("Executing run wrapper: '%s'", fn->name.data);
#endif // NDEBUG

    Interpreter run_interp = interpreter_create(c_allocator(), bc->context);
    defer { interpreter_free(&run_interp); };

    auto run_prog = bytecode_get_program(bc->builder);

    run_interp.std_out = stdout_file;
    Interpreter_Register result = interpreter_start(&run_interp, run_prog, fn_handle);

    return result;
}

AST_Expression *interpreter_register_to_ast_expression(Bytecode_Converter *bc, Interpreter_Register &reg, Scope *scope, Source_Range range)
{
    debug_assert(bc);
    debug_assert(reg.type);

    auto ctx = bc->context;

    AST_Expression *result = nullptr;
    Infer_Node infer_node = create_infer_node(reg.type);

    switch (reg.type->kind) {

        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
             result = ast_integer_literal_expr_new(ctx, range, reg.value.integer);
             break;
        }

        case Type_Kind::FLOAT: {
            result = ast_real_literal_expr_new(ctx, range, reg.value.real);
            break;
        }

        case Type_Kind::BOOLEAN: {
            result = ast_bool_literal_expr_new(ctx, range, reg.value.boolean);
            break;
        }

        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::STRUCTURE: assert(false); break;
        case Type_Kind::STATIC_ARRAY: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
    }

    assert(result);

#ifndef NDEBUG
    bool resolved_name =
#endif // NDEBUG
        name_resolve_expr(ctx, result, scope);
    debug_assert(resolved_name);

#ifndef NDEBUG
    bool resolved_type =
#endif // NDEBUg
        type_resolve_expression(ctx, result, scope, infer_node);
    debug_assert(resolved_type);


    return result;
}

} }
