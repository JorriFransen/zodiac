#include "bytecode/converter.h"

#include "ast.h"
#include "atom.h"
#include "containers/dynamic_array.h"
#include "error.h"
#include "resolve.h"
#include "scope.h"
#include "type.h"
#include "util/asserts.h"

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

    return result;
}

void emit_bytecode(Resolver *resolver, Bytecode_Converter *bc)
{
    assert(resolver);
    assert(bc);

    for (s64 i = 0; i < resolver->nodes_to_emit_bytecode.count; i++) {

        Flat_Root_Node root_node = resolver->nodes_to_emit_bytecode[i];

        switch (root_node.root.kind) {

            case Flat_Node_Kind::DECL: {
                ast_decl_to_bytecode(bc, root_node.root.decl);
                break;
            }

            case Flat_Node_Kind::STMT: assert(false); break;
            case Flat_Node_Kind::EXPR: assert(false); break;
            case Flat_Node_Kind::TS: assert(false); break;
            case Flat_Node_Kind::PARAM_DECL: assert(false); break;
            case Flat_Node_Kind::FIELD_DECL: assert(false); break;

            case Flat_Node_Kind::FUNCTION_PROTO: break;
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

            if (decl->flags & AST_DECL_FLAG_GLOBAL) {

                Type *global_type = decl->variable.resolved_type;
                assert(global_type);
                auto global_name = decl->identifier.name;

                Bytecode_Register initial_value_reg;
                if (decl->variable.value) {
                    initial_value_reg = ast_expr_to_bytecode(bc, decl->variable.value, decl->variable.resolved_type);
                }

                auto global_handle = bytecode_create_global(bc->builder, global_name, global_type, initial_value_reg);

                hash_table_add(&bc->globals, decl, global_handle);

            } else {
                // Leaf
            }
            break;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {

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

        case AST_Declaration_Kind::FUNCTION: {
            ast_function_to_bytecode(bc, decl);
            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false); break;
        case AST_Declaration_Kind::UNION: assert(false); break;
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

            if (var_decl->variable.value) has_inits = true;
        }

        if (has_inits) {
            Bytecode_Block_Handle inits_block_handle = bytecode_append_block(bc->builder, fn_handle, "inits");
            bytecode_emit_jmp(bc->builder, inits_block_handle);

            bytecode_set_insert_point(bc->builder, fn_handle, inits_block_handle);

            for (s64 i = 0; i < decl->function.variables.count; i++) {
                AST_Declaration *var_decl = decl->function.variables[i];
                if (var_decl->variable.value) {
                    Bytecode_Register value = ast_expr_to_bytecode(bc, var_decl->variable.value, var_decl->variable.resolved_type);

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

    auto fn = bc->builder->functions[fn_handle];
    auto block = &fn.blocks[bc->builder->insert_block_index];

    if (fn.type->function.return_type->kind == Type_Kind::VOID) {
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
        case AST_Statement_Kind::BLOCK: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_bytecode(bc, stmt->decl.decl);
            break;
        }

        case AST_Statement_Kind::ASSIGN: assert(false); break;
        case AST_Statement_Kind::CALL: assert(false); break;
        case AST_Statement_Kind::IF: assert(false); break;
        case AST_Statement_Kind::WHILE: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                assert(stmt->return_stmt.scope);
                AST_Declaration *func_decl = enclosing_function(stmt->return_stmt.scope);
                assert(func_decl);

                assert(func_decl->function.type);
                Type *return_type = func_decl->function.type->function.return_type;

                Bytecode_Register return_value_register = ast_expr_to_bytecode(bc, stmt->return_stmt.value, return_type);
                bytecode_emit_return(bc->builder, return_value_register);
            } else {
                bytecode_emit_return(bc->builder);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            assert(stmt->print_expr->resolved_type->kind == Type_Kind::INTEGER);
            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, stmt->print_expr);
            bytecode_emit_print(bc->builder, value_reg);
            break;
        }
    }
}

Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type *actual_type/*=nullptr*/)
{
    assert(bc);
    assert(expr);

    if (actual_type) {
        assert(expr->resolved_type == &builtin_type_unsized_integer ||
               expr->resolved_type == actual_type);
        assert(actual_type != &builtin_type_unsized_integer);
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            Type *literal_type = expr->resolved_type;
            if (literal_type->kind == Type_Kind::UNSIZED_INTEGER) {
                assert(actual_type);
                literal_type = actual_type;
            }
            assert(literal_type->kind == Type_Kind::INTEGER);
            return bytecode_integer_literal(bc->builder, literal_type, expr->integer_literal.value);
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

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

                    if (ident_decl->flags & AST_DECL_FLAG_GLOBAL) {

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
                    Type *type = expr->resolved_type;
                    if (ident_decl->variable.resolved_type == &builtin_type_unsized_integer) {
                        assert(actual_type);
                        type = actual_type;
                    }
                    return ast_expr_to_bytecode(bc, ident_decl->variable.value, type);
                }

                case AST_Declaration_Kind::FUNCTION: {
                    // At this point this must a function paramter
                    // TODO: Fix this, paramter symbols should point to a parameter declaration?
                    assert(ident_sym->kind == Symbol_Kind::PARAM);

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

                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
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

            if (actual_type) {
                assert(actual_type == expr->resolved_type);
            } else {
                actual_type = expr->resolved_type;
            }
            assert(actual_type->kind == Type_Kind::INTEGER);

            Bytecode_Register lhs_reg = ast_expr_to_bytecode(bc, expr->binary.lhs, actual_type);
            Bytecode_Register rhs_reg = ast_expr_to_bytecode(bc, expr->binary.rhs, actual_type);

            switch (expr->binary.op) {
                case AST_Binary_Operator::INVALID: assert(false); break;

                case AST_Binary_Operator::ADD: {
                    return bytecode_emit_add(bc->builder, lhs_reg, rhs_reg);
                }

                case AST_Binary_Operator::SUB: assert(false); break;
                case AST_Binary_Operator::MUL: assert(false); break;
                case AST_Binary_Operator::DIV: assert(false); break;
                case AST_Binary_Operator::EQ: assert(false); break;
                case AST_Binary_Operator::NEQ: assert(false); break;
                case AST_Binary_Operator::LT: assert(false); break;
                case AST_Binary_Operator::GT: assert(false); break;
                case AST_Binary_Operator::LTEQ: assert(false); break;
                case AST_Binary_Operator::GTEQ: assert(false); break;
            }
            break;
        }
    }

    assert(false);
    return {};
}
} }
