#include "bytecode/converter.h"

#include "ast.h"
#include "atom.h"
#include "bytecode/ffi.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "common.h"
#include "constant_resolver.h"
#include "containers/dynamic_array.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "type_info.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

namespace Zodiac { namespace Bytecode {

Bytecode_Converter bytecode_converter_create(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb)
{
    Bytecode_Converter result = {};
    bytecode_converter_init(allocator, context, bb, &result);
    return result;
}

void bytecode_converter_init(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb, Bytecode_Converter *out_bc)
{
    debug_assert(allocator && context && bb && out_bc);

    out_bc->allocator = allocator;
    out_bc->context = context;
    out_bc->builder = bb;

    hash_table_create(allocator, &out_bc->functions);
    stack_init(allocator, &out_bc->defer_stack);
    stack_init(allocator, &out_bc->switch_case_stack);
    hash_table_create(allocator, &out_bc->allocations);
    hash_table_create(allocator, &out_bc->implicit_lvalues);
    hash_table_create(allocator, &out_bc->globals);
    hash_table_create(allocator, &out_bc->run_directives);
    hash_table_create(allocator, &out_bc->run_results);

    out_bc->run_directive_count = 0;
}

void bytecode_converter_destroy(Bytecode_Converter *bc)
{
    hash_table_free(&bc->functions);
    hash_table_free(&bc->allocations);
    hash_table_free(&bc->globals);
    hash_table_free(&bc->run_directives);
    hash_table_free(&bc->run_results);
}

bool emit_bytecode(Resolver *resolver, Bytecode_Converter *bc)
{
    assert(resolver);
    assert(bc);

    bool result = true;

    for (s64 i = 0; i < resolver->nodes_to_emit_bytecode.count; i++) {

        Flat_Root_Node *root_node = resolver->nodes_to_emit_bytecode[i];

        switch (root_node->root.kind) {

            case Flat_Node_Kind::DECL: {
                bool decl_res = ast_decl_to_bytecode(bc, root_node->root.decl);
                if (decl_res && root_node->root.decl->kind == AST_Declaration_Kind::RUN_DIRECTIVE) {

                        dynamic_array_append(&resolver->nodes_to_run_bytecode, root_node);
                }

                result = decl_res;
                break;
            }

            case Flat_Node_Kind::STMT: assert(false); break;
            case Flat_Node_Kind::EXPR: assert(false); break;
            case Flat_Node_Kind::TYPE_SPEC: assert(false); break;

            case Flat_Node_Kind::IMPLICIT_LVALUE: {
                auto implicit_lval = root_node->root.implicit_lvalue;

                auto expr = implicit_lval.expr;

                auto ta = temp_allocator();
                auto taa = temp_allocator_allocator();
                auto mark = temporary_allocator_get_mark(ta);


                if (implicit_lval.kind == AST_Implicit_LValue_Kind::CONST_LVALUE) {
                    auto decl = implicit_lval.decl;
                    assert(expr->kind == AST_Expression_Kind::IDENTIFIER);
                    auto sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
                    assert(sym->decl && sym->decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);
                    auto const_decl = sym->decl;

                    assert(const_decl->variable.value);
                    Bytecode_Register value_reg = ast_expr_to_bytecode(bc, const_decl->variable.value);

                    Type *alloc_type = expr->resolved_type;
                    auto alloc_name = atom_get(&bc->context->atoms, string_append(taa, "clv_", decl->identifier.name.data));
                    alloc_name = bytecode_unique_global_name(bc->builder, alloc_name);
                    bytecode_create_global(bc->builder, alloc_name, alloc_type, false, value_reg);

                    temporary_allocator_reset(ta, mark);

                    Bytecode_Register global_reg;
                    bool found = hash_table_find(&bc->builder->global_registers, alloc_name, &global_reg);
                    assert(found);

                    hash_table_add(&bc->implicit_lvalues, expr, global_reg);
                } else {
                    assert(implicit_lval.kind == AST_Implicit_LValue_Kind::SLICE_ARRAY);

                    Atom array_alloc_name = atom_get(&bc->context->atoms, "scs");
                    array_alloc_name = bytecode_unique_global_name(bc->builder, array_alloc_name);

                    Type *array_type = implicit_lval.expr->resolved_type;

                    Bytecode_Register value_reg = ast_const_expr_to_bytecode(bc, implicit_lval.expr);
                    bytecode_create_global(bc->builder, array_alloc_name, array_type, false, value_reg);

                    Bytecode_Register global_reg;
                    bool found = hash_table_find(&bc->builder->global_registers, array_alloc_name, &global_reg);
                    assert(found);

                    Bytecode_Register length_reg = bytecode_integer_literal(bc->builder, &builtin_type_s64, array_type->static_array.count);

                    Bytecode_Register members[2] = { global_reg, length_reg };
                    Bytecode_Register slice_reg = bytecode_aggregate_literal(bc->builder, members, implicit_lval.slice.type->slice.struct_type);

                    hash_table_add(&bc->implicit_lvalues, expr, slice_reg);
                }
                break;
            };

            case Flat_Node_Kind::FUNCTION_PROTO: {
                auto decl = root_node->root.decl;
                assert(decl->kind == AST_Declaration_Kind::FUNCTION);

                BC_Function_Flag flags = BC_FUNCTION_FLAG_NONE;

                bool foreign = decl->flags & AST_DECL_FLAG_FOREIGN;

                if (foreign) {
                    flags |= BC_FUNCTION_FLAG_FOREIGN;
                }

                bool found = hash_table_find(&bc->functions, decl);
                assert(!found);

                auto fn_handle = bytecode_function_create(bc->builder, decl->identifier.name, decl->function.type, flags);

                hash_table_add(&bc->functions, decl, fn_handle);
                break;
            }

            case Flat_Node_Kind::RUN: {
                assert(root_node->root.run.expr->kind == AST_Expression_Kind::RUN_DIRECTIVE);
                auto directive = root_node->root.run.expr->directive;

                Bytecode_Function_Handle fn_handle = create_run_wrapper(bc, directive.directive);
                if (fn_handle < 0) {
                    return false;
                }

                debug_assert(!hash_table_find(&bc->run_directives, directive.directive));
                hash_table_add(&bc->run_directives, directive.directive, fn_handle);

                dynamic_array_append(&resolver->nodes_to_run_bytecode, root_node);
                break;
            }
        }

        dynamic_array_remove_ordered(&resolver->nodes_to_emit_bytecode, i);
        i -= 1;

        if (!result) break;
    }

    return result;
}

bool ast_decl_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl)
{
    assert(bc);
    assert(decl);
    assert(decl->kind != AST_Declaration_Kind::INVALID);

    if (decl->flags & AST_DECL_FLAG_TYPE_DECL) {
        return true;
    }

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (DECL_IS_GLOBAL(decl)) {

                Type *global_type = decl->variable.resolved_type;

                auto global_name = decl->identifier.name;
                global_name = bytecode_unique_global_name(bc->builder, global_name);

                Bytecode_Register initial_value_reg = {};
                auto value_expr = decl->variable.value;
                if (value_expr) {
                    if (value_expr->resolved_type->kind == Type_Kind::STATIC_ARRAY && global_type->kind == Type_Kind::SLICE) {

                        bool found = hash_table_find(&bc->implicit_lvalues, value_expr, &initial_value_reg);
                        assert(found);
                    } else {
                        initial_value_reg = ast_expr_to_bytecode(bc, decl->variable.value);
                    }
                } else {
                    initial_value_reg = bytecode_zero_value(bc->builder, decl->variable.resolved_type);
                }

                auto global_handle = bytecode_create_global(bc->builder, global_name, global_type, false, initial_value_reg);

                hash_table_add(&bc->globals, decl, global_handle);

            } else {
                if (decl->variable.value) {

                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, decl, &alloc_reg);
                    assert(found)

                    if (!EXPR_IS_CONST(decl->variable.value) || decl->variable.value->kind == AST_Expression_Kind::RUN_DIRECTIVE) { // Constant should have been emitted when registering the allocation
                        assignment_to_bytecode(bc, decl->variable.value, alloc_reg);
                    }
                }
            }
            break;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {

            auto name = bytecode_unique_global_name(bc->builder, decl->identifier.name);
            auto type = decl->variable.resolved_type;
            auto value = decl->variable.value;

            Bytecode_Register initial_value_reg;
            if (type->kind == Type_Kind::SLICE && value->resolved_type->kind == Type_Kind::STATIC_ARRAY) {

                Bytecode_Register pointer_reg;
                if (value->kind == AST_Expression_Kind::COMPOUND) {

                    Atom array_name = bytecode_unique_global_name(bc->builder, atom_get(&bc->context->atoms, "const_slice_array"));
                    Bytecode_Register array_reg = ast_const_expr_to_bytecode(bc, value);
                    bytecode_create_global(bc->builder, array_name, value->resolved_type, true, array_reg);

                    bool reg_found = hash_table_find(&bc->builder->global_registers, array_name, &pointer_reg);
                    assert(reg_found);

                } else {

                    pointer_reg = ast_const_lvalue_to_bytecode(bc, value);
                    assert(pointer_reg.kind == Bytecode_Register_Kind::ALLOC || pointer_reg.kind == Bytecode_Register_Kind::GLOBAL);
                }

                Bytecode_Register length_reg = bytecode_integer_literal(bc->builder, &builtin_type_s64, value->resolved_type->static_array.count);

                Bytecode_Register members[2] = { pointer_reg, length_reg };
                Bytecode_Register slice_reg = bytecode_aggregate_literal(bc->builder, members, type->slice.struct_type);
                slice_reg.type = type;

                Bytecode_Global_Handle global_handle = bytecode_create_global(bc->builder, name, slice_reg.type, true, slice_reg);

                hash_table_add(&bc->globals, decl, global_handle);
            } else {
                initial_value_reg = ast_expr_to_bytecode(bc, decl->variable.value);

                assert(initial_value_reg.type == type ||
                       initial_value_reg.type->kind == Type_Kind::INTEGER && type->kind == Type_Kind::UNSIZED_INTEGER)
                auto global_handle = bytecode_create_global(bc->builder, name, initial_value_reg.type, true, initial_value_reg);

                hash_table_add(&bc->globals, decl, global_handle);
            }
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
        case AST_Declaration_Kind::ENUM: {
            // leaf
            break;
        }

        case AST_Declaration_Kind::ENUM_MEMBER: assert(false); break;

        case AST_Declaration_Kind::RUN_DIRECTIVE: {

            auto directive = decl->directive;
            Bytecode_Function_Handle fn_handle = create_run_wrapper(bc, directive);
            if (fn_handle < 0) {
                return false;
            }

            debug_assert(!hash_table_find(&bc->run_directives, directive));
            hash_table_add(&bc->run_directives, directive, fn_handle);

            break;
        }

        case AST_Declaration_Kind::IMPORT_DIRECTIVE: {
            break;
        }
    }

    return true;
}

void ast_function_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl)
{
    assert(bc);
    assert(decl);
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    assert(decl->function.type);
    assert(decl->function.type->kind == Type_Kind::FUNCTION);
    assert(decl->identifier.name.data);

    Bytecode_Function_Handle fn_handle;
    bool found = hash_table_find(&bc->functions, decl, &fn_handle);
    assert(found);

    bool foreign = decl->flags & AST_DECL_FLAG_FOREIGN;

    if (foreign) {
        return;
    }

    auto taa = temp_allocator_allocator();
    auto ta = temp_allocator();
    auto mark = temporary_allocator_get_mark(ta);

    if (decl->function.variables.count || decl->function.params.count || decl->function.implicit_lvalues.count) {

        Bytecode_Block_Handle allocs_block_handle = bytecode_append_block(bc->builder, fn_handle, "allocs");
        bytecode_set_insert_point(bc->builder, fn_handle, allocs_block_handle);

        bool has_inits = false;

        for (s64 i = 0; i < decl->function.params.count; i++) {
            AST_Declaration *param_decl = decl->function.params[i];
            assert(param_decl->variable.resolved_type);

            Type *param_type = param_decl->variable.resolved_type;
            auto alloc_name = string_append(taa, "arg_", param_decl->identifier.name.data);
            alloc_name = bytecode_unique_register_name_in_function(bc->builder, fn_handle, alloc_name);
            auto alloc_reg = bytecode_emit_alloc(bc->builder, param_type, alloc_name.data);

            debug_assert(!hash_table_find(&bc->allocations, param_decl));
            hash_table_add(&bc->allocations, param_decl, alloc_reg);
        }

        for (s64 i = 0; i < decl->function.variables.count; i++) {
            AST_Declaration *var_decl = decl->function.variables[i];
            assert(var_decl->variable.resolved_type);

            Type *alloc_type = var_decl->variable.resolved_type;

            auto alloc_name = String_Ref(var_decl->identifier.name);
            alloc_name = bytecode_unique_register_name_in_function(bc->builder, fn_handle, alloc_name);
            auto alloc_reg = bytecode_emit_alloc(bc->builder, alloc_type, alloc_name.data);

            debug_assert(!hash_table_find(&bc->allocations, var_decl));

            hash_table_add(&bc->allocations, var_decl, alloc_reg);

            if (var_decl->variable.value && EXPR_IS_CONST(var_decl->variable.value)) has_inits = true;
            if (!var_decl->variable.value) has_inits = true; // For zero init
        }

        auto mark = temporary_allocator_get_mark(temp_allocator());
        Hash_Table<AST_Expression *, Bytecode_Register> slice_compound_arrays;
        hash_table_create(temp_allocator_allocator(), &slice_compound_arrays);

        for (s64 i = 0; i < decl->function.implicit_lvalues.count; i++) {
            auto implicit_lval = decl->function.implicit_lvalues[i];

            Bytecode_Register alloc_reg;
            if (implicit_lval.kind == AST_Implicit_LValue_Kind::CONST_LVALUE) {

                Type *alloc_type = implicit_lval.expr->resolved_type;
                auto alloc_name = string_append(taa, "clv_", implicit_lval.decl->identifier.name.data);
                alloc_name = bytecode_unique_register_name_in_function(bc->builder, fn_handle, alloc_name);
                alloc_reg = bytecode_emit_alloc(bc->builder, alloc_type, alloc_name.data);
                hash_table_add(&bc->implicit_lvalues, implicit_lval.expr, alloc_reg);

            } else {
                assert(implicit_lval.kind == AST_Implicit_LValue_Kind::SLICE_ARRAY);

                Type *array_alloc_type = implicit_lval.expr->resolved_type;

                if (implicit_lval.slice.needs_local_array_alloc) {

                    assert(!implicit_lval.slice.needs_global_array_alloc)

                    auto array_alloc_name = bytecode_unique_register_name_in_function(bc->builder, fn_handle, "slice_array_storage");
                    Bytecode_Register array_alloc_reg = bytecode_emit_alloc(bc->builder, array_alloc_type, array_alloc_name.data);

                    hash_table_add(&slice_compound_arrays, implicit_lval.expr, array_alloc_reg);

                } else if (implicit_lval.slice.needs_global_array_alloc) {

                    assert(!implicit_lval.slice.needs_local_array_alloc)

                    auto array_global_name = bytecode_unique_global_name(bc->builder, atom_get(&bc->context->atoms, "slice_array_storage"));

                    auto value = ast_expr_to_bytecode(bc, implicit_lval.expr);
                    bytecode_create_global(bc->builder, array_global_name, array_alloc_type, false, value);

                    Bytecode_Register global_reg;
                    bool found = hash_table_find(&bc->builder->global_registers, array_global_name, &global_reg);
                    assert(found);

                    hash_table_add(&bc->implicit_lvalues, implicit_lval.expr, global_reg);
                }
            }

            has_inits = true;
        }

        for (s64 i = 0; i < decl->function.params.count; i++) {
            auto param_decl = decl->function.params[i];

            Bytecode_Register alloc_reg;
            bool found = hash_table_find(&bc->allocations, param_decl, &alloc_reg);
            assert(found);

            auto arg_reg = bytecode_emit_load_argument(bc->builder, i);
            bytecode_emit_store_alloc(bc->builder, arg_reg, alloc_reg);
        }

        if (has_inits) {
            Bytecode_Block_Handle inits_block_handle = bytecode_append_block(bc->builder, fn_handle, "inits");
            bytecode_emit_jmp(bc->builder, inits_block_handle);

            bytecode_set_insert_point(bc->builder, fn_handle, inits_block_handle);

            for (s64 i = 0; i < decl->function.implicit_lvalues.count; i++) {
                auto implicit_lvalue = decl->function.implicit_lvalues[i];
                auto expr = implicit_lvalue.expr;

                if (implicit_lvalue.kind == AST_Implicit_LValue_Kind::CONST_LVALUE) {

                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->implicit_lvalues, expr, &alloc_reg);
                    assert(found);

                    assert(expr->kind == AST_Expression_Kind::IDENTIFIER);
                    auto sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
                    assert(sym->decl);
                    assert(sym->decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);

                    auto const_decl = sym->decl;

                    assert(const_decl->variable.value);
                    assignment_to_bytecode(bc, const_decl->variable.value, alloc_reg);

                } else {
                    assert(implicit_lvalue.kind == AST_Implicit_LValue_Kind::SLICE_ARRAY);


                    if (implicit_lvalue.slice.needs_local_array_alloc && EXPR_IS_CONST(expr)) {
                        Bytecode_Register array_alloc;
                        bool found = hash_table_find(&slice_compound_arrays, expr, &array_alloc);
                        assert(found);

                        assignment_to_bytecode(bc, expr, array_alloc);
                        hash_table_add(&bc->implicit_lvalues, expr, array_alloc);
                    }
                }
            }

            for (s64 i = 0; i < decl->function.variables.count; i++) {
                AST_Declaration *var_decl = decl->function.variables[i];
                if (var_decl->variable.value && EXPR_IS_CONST(var_decl->variable.value) && var_decl->variable.value->kind != AST_Expression_Kind::RUN_DIRECTIVE) {

                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, var_decl, &alloc_reg);
                    assert(found)

                    assignment_to_bytecode(bc, var_decl->variable.value, alloc_reg);
                } else if (!var_decl->variable.value) {

                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, var_decl, &alloc_reg);
                    assert(found)

                    Bytecode_Register zero_value = bytecode_zero_value(bc->builder, var_decl->variable.resolved_type);
                    bytecode_emit_store_alloc(bc->builder, zero_value, alloc_reg);
                }
            }
        }

        for (s64 i = 0; i < decl->function.implicit_lvalues.count; i++) {
            auto implicit_lvalue = decl->function.implicit_lvalues[i];
            auto expr = implicit_lvalue.expr;

            if (implicit_lvalue.kind == AST_Implicit_LValue_Kind::SLICE_ARRAY) {
                if (implicit_lvalue.slice.needs_local_array_alloc && !EXPR_IS_CONST(expr)) {
                    Bytecode_Register array_alloc;
                    bool found = hash_table_find(&slice_compound_arrays, expr, &array_alloc);
                    assert(found);

                    assignment_to_bytecode(bc, expr, array_alloc);
                    hash_table_add(&bc->implicit_lvalues, expr, array_alloc);
                }
            }
        }

        temporary_allocator_reset(temp_allocator(), mark);

        Bytecode_Block_Handle start_block_handle = bytecode_append_block(bc->builder, fn_handle, "start");
        bytecode_emit_jmp(bc->builder, start_block_handle);
        bytecode_set_insert_point(bc->builder, fn_handle, start_block_handle);
    } else {
        Bytecode_Block_Handle entry_block_handle = bytecode_append_block(bc->builder, fn_handle, "entry");
        bytecode_set_insert_point(bc->builder, fn_handle, entry_block_handle);
    }

    temporary_allocator_reset(ta, mark);

    assert(stack_count(&bc->defer_stack) == 0);

    auto local_scope = decl->function.local_scope;
    assert(local_scope->kind == Scope_Kind::FUNCTION_LOCAL);

    // Means we have returned from the body block directly, NOT from one of it's children
    bool body_returned_directly = false;

    bool block_terminated = false;

    for (s64 i = 0; i < decl->function.body.count; i++) {

        AST_Statement *stmt = decl->function.body[i];

        ast_stmt_to_bytecode(bc, stmt);

        if (stmt->kind == AST_Statement_Kind::RETURN) {
            body_returned_directly = true;
            break;
        }

        if (bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
            block_terminated = true;
            break;
        }
    }

    if (!body_returned_directly && !block_terminated) {
        for (s64 i = local_scope->func.defer_stmts.count - 1; i >= 0; i -= 1) {
            auto defer_stmt = local_scope->func.defer_stmts[i];
            assert(defer_stmt->kind == AST_Statement_Kind::DEFER);
            ast_stmt_to_bytecode(bc, defer_stmt->defer_stmt.stmt);
        }
    }

    assert(stack_count(&bc->defer_stack) == local_scope->func.defer_stmts.count);
    bc->defer_stack.sp = 0;

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

    decl->flags |= AST_DECL_FLAG_BYTECODE_EMITTED;
}

bool ast_stmt_to_bytecode(Bytecode_Converter *bc, AST_Statement *stmt)
{
    assert(bc);
    assert(stmt);

    auto cfn = (Bytecode_Function_Handle)bc->builder->insert_fn_index;

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::BLOCK: {
            auto scope = stmt->block.scope;
            assert(scope->kind == Scope_Kind::FUNCTION_LOCAL);

            auto old_defer_sp = bc->defer_stack.sp;

            // Means we have returned from the block directly, NOT from one of it's children
            bool block_returned_directly = false;

            bool block_terminated = false;

            for (s64 i = 0; i < stmt->block.statements.count; i++) {

                ast_stmt_to_bytecode(bc, stmt->block.statements[i]);

                if (stmt->block.statements[i]->kind == AST_Statement_Kind::RETURN) {
                    block_returned_directly = true;
                    break;
                }

                if (bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
                    block_terminated = true;
                    break;
                }
            }

            bc->defer_stack.sp = old_defer_sp;

            if (!block_returned_directly && !block_terminated) {
                for (s64 i = scope->func.defer_stmts.count - 1; i >= 0; i -= 1) {
                    auto defer_stmt = scope->func.defer_stmts[i];
                    assert(defer_stmt->kind == AST_Statement_Kind::DEFER);
                    ast_stmt_to_bytecode(bc, defer_stmt->defer_stmt.stmt);
                }
            }
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            return ast_decl_to_bytecode(bc, stmt->decl.decl);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            Bytecode_Register lvalue_reg = ast_lvalue_to_bytecode(bc, stmt->assign.dest);

            if (stmt->flags & AST_STMT_FLAG_COMPOUND_ASSIGNMENT) {
                Bytecode_Register lhs_reg = bytecode_emit_load(bc->builder, lvalue_reg);
                auto binary = stmt->assign.value;
                assert(binary->kind == AST_Expression_Kind::BINARY);
                Bytecode_Register rhs_reg = ast_expr_to_bytecode(bc, binary->binary.rhs);

                Bytecode_Register new_value;
                switch (binary->binary.op) {

                    case AST_Binary_Operator::ADD: {
                        new_value = bytecode_emit_add(bc->builder, lhs_reg, rhs_reg);
                        break;
                    }
                    case AST_Binary_Operator::SUB: {
                        new_value = bytecode_emit_sub(bc->builder, lhs_reg, rhs_reg);
                        break;
                    }
                    case AST_Binary_Operator::MUL: {
                        new_value = bytecode_emit_mul(bc->builder, lhs_reg, rhs_reg);
                        break;
                    }
                    case AST_Binary_Operator::DIV: {
                        new_value = bytecode_emit_div(bc->builder, lhs_reg, rhs_reg);
                        break;
                    }
                    default: assert(false);
                }

                bytecode_emit_store(bc->builder, new_value, lvalue_reg);

            } else {
                assignment_to_bytecode(bc, stmt->assign.value, lvalue_reg);
            }
            break;
        }

        case AST_Statement_Kind::CALL: {
            ast_expr_to_bytecode(bc, stmt->call.call);
            break;
        }

        case AST_Statement_Kind::IF: {
            auto ta = temp_allocator_allocator();

            auto temp_blocks = temp_array_create<Bytecode_Block_Handle>(ta);

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                if (i > 0) {
                    auto elif_handle = bytecode_create_block(bc->builder, cfn, "elif_cond");
                    dynamic_array_append(&temp_blocks.array, elif_handle);
                }
                auto block_handle = bytecode_create_block(bc->builder, cfn, "then");
                dynamic_array_append(&temp_blocks.array, block_handle);
            }

            if (stmt->if_stmt.else_stmt) {
                auto block_handle = bytecode_create_block(bc->builder, cfn, "else");
                dynamic_array_append(&temp_blocks.array, block_handle);
            }

            auto post_if_block_handle = bytecode_create_block(bc->builder, cfn, "post_if");
            dynamic_array_append(&temp_blocks.array, post_if_block_handle);

            auto block_index = 0;
            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = &stmt->if_stmt.blocks[i];

                if (i > 0) {
                    // The first condition doesn't need it's own block
                    bytecode_append_block(bc->builder, cfn, temp_blocks[block_index]);
                    bytecode_set_insert_point(bc->builder, cfn, temp_blocks.array[block_index++]);
                }
                Bytecode_Register cond_reg = ast_expr_to_bytecode(bc, if_block->cond);

                auto then_block = temp_blocks.array[block_index++];
                auto else_block = temp_blocks.array[block_index];

                bytecode_emit_jmp_if(bc->builder, cond_reg, then_block, else_block);

                bytecode_append_block(bc->builder, cfn, then_block);
                bytecode_set_insert_point(bc->builder, cfn, then_block);
                ast_stmt_to_bytecode(bc, if_block->then);

                if (!bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
                    bytecode_emit_jmp(bc->builder, post_if_block_handle);
                }

            }

            if (stmt->if_stmt.else_stmt) {

                assert(temp_blocks.array.count >= 2);
                auto else_block = temp_blocks.array[temp_blocks.array.count - 2];

                bytecode_append_block(bc->builder, cfn, else_block);
                bytecode_set_insert_point(bc->builder, cfn, else_block);
                ast_stmt_to_bytecode(bc, stmt->if_stmt.else_stmt);

                if (!bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
                    bytecode_emit_jmp(bc->builder, post_if_block_handle);
                }
            }

            bytecode_append_block(bc->builder, cfn, post_if_block_handle);
            bytecode_set_insert_point(bc->builder, cfn, post_if_block_handle);

            temp_array_destroy(&temp_blocks);

            break;
        }

        case AST_Statement_Kind::WHILE: {

            auto while_cond_block = bytecode_append_block(bc->builder, cfn, "while.cond");
            auto while_body_block = bytecode_create_block(bc->builder, cfn, "while.body");
            auto post_while_block = bytecode_create_block(bc->builder, cfn, "while.post");

            bytecode_emit_jmp(bc->builder, while_cond_block);

            bytecode_set_insert_point(bc->builder, cfn, while_cond_block);
            auto cond_reg = ast_expr_to_bytecode(bc, stmt->while_stmt.cond);
            bytecode_emit_jmp_if(bc->builder, cond_reg, while_body_block, post_while_block);

            while_body_block = bytecode_append_block(bc->builder, cfn, while_body_block);
            bytecode_set_insert_point(bc->builder, cfn, while_body_block);
            ast_stmt_to_bytecode(bc, stmt->while_stmt.body_stmt);

            if (!bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
                bytecode_emit_jmp(bc->builder, while_cond_block);
            }

            post_while_block = bytecode_append_block(bc->builder, cfn, post_while_block);
            bytecode_set_insert_point(bc->builder, cfn, post_while_block);
            break;
        }


        case AST_Statement_Kind::FOR: {

            auto for_cond_block = bytecode_append_block(bc->builder, cfn, "for.cond");
            auto for_body_block = bytecode_create_block(bc->builder, cfn, "for.body");
            auto post_for_block = bytecode_create_block(bc->builder, cfn, "for.post");

            ast_decl_to_bytecode(bc, stmt->for_stmt.init_decl);

            bytecode_emit_jmp(bc->builder, for_cond_block);

            bytecode_set_insert_point(bc->builder, cfn, for_cond_block);
            auto cond_reg = ast_expr_to_bytecode(bc, stmt->for_stmt.cond_expr);
            bytecode_emit_jmp_if(bc->builder, cond_reg, for_body_block, post_for_block);

            for_body_block = bytecode_append_block(bc->builder, cfn, for_body_block);
            bytecode_set_insert_point(bc->builder, cfn, for_body_block);
            ast_stmt_to_bytecode(bc, stmt->for_stmt.body_stmt);

            if (!bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
                ast_stmt_to_bytecode(bc, stmt->for_stmt.inc_stmt);
                bytecode_emit_jmp(bc->builder, for_cond_block);
            }


            post_for_block = bytecode_append_block(bc->builder, cfn, post_for_block);
            bytecode_set_insert_point(bc->builder, cfn, post_for_block);
            break;
        }

        case AST_Statement_Kind::SWITCH: {

            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, stmt->switch_stmt.value);

            auto initial_block_handle = bc->builder->insert_block_index;

            Dynamic_Array<Bytecode_Switch_Case> bc_cases;
            dynamic_array_create(bc->allocator, &bc_cases, stmt->switch_stmt.cases.count);

            auto post_switch_block = bytecode_create_block(bc->builder, cfn, "switch.post");
            auto default_or_post_block = post_switch_block;

            auto blocks = temp_array_create<Bytecode_Block_Handle>(temp_allocator_allocator(), stmt->switch_stmt.cases.count);

            for (s64 i = 0; i < stmt->switch_stmt.cases.count; i++) {
                auto block_handle = bytecode_create_block(bc->builder, cfn, "switch.case");
                dynamic_array_append(&blocks, block_handle);
            }

            for (s64 case_index = 0; case_index < stmt->switch_stmt.cases.count; case_index++) {

                auto case_stmt = stmt->switch_stmt.cases[case_index];

                auto case_block = blocks[case_index];
                bytecode_append_block(bc->builder, cfn, case_block);
                bytecode_set_insert_point(bc->builder, cfn, case_block);

                if (case_stmt->switch_case_stmt.is_default) {
                    default_or_post_block = case_block;
                }

                stack_push(&bc->switch_case_stack, { stmt, case_index, blocks, post_switch_block });

                ast_stmt_to_bytecode(bc, case_stmt->switch_case_stmt.case_stmt);

                stack_pop(&bc->switch_case_stack);

                if (!bytecode_block_is_terminated(bytecode_get_insert_block(bc->builder))) {
                    bytecode_emit_jmp(bc->builder, post_switch_block);
                }

                auto case_values = case_stmt->switch_case_stmt.case_values;

                if (case_values.count) {
                    assert(!case_stmt->switch_case_stmt.is_default);

                    for (s64 case_value_index = 0; case_value_index < case_values.count; case_value_index++) {
                        auto case_value = case_values[case_value_index];

                        if (case_value->kind == AST_Expression_Kind::RANGE) {
                            assert(case_value->resolved_type->kind == Type_Kind::INTEGER);

                            for (s64 i = case_value->range.min_value; i <= case_value->range.max_value; i++) {
                                Bytecode_Register case_value_reg = bytecode_integer_literal(bc->builder, case_value->resolved_type, i);

                                Bytecode_Switch_Case bc_case = { case_value_reg, bytecode_block_value(bc->builder, case_block), false };
                                dynamic_array_append(&bc_cases, bc_case);
                            }

                        } else {
                            Bytecode_Register case_value_reg = ast_expr_to_bytecode(bc, case_value);

                            Bytecode_Switch_Case bc_case = { case_value_reg, bytecode_block_value(bc->builder, case_block), false };
                            dynamic_array_append(&bc_cases, bc_case);
                        }
                    }
                } else {
                    assert(case_stmt->switch_case_stmt.is_default);
                    Bytecode_Switch_Case bc_case = { {}, bytecode_block_value(bc->builder, case_block), true };
                    dynamic_array_append(&bc_cases, bc_case);
                }

            }

            bytecode_set_insert_point(bc->builder, cfn, initial_block_handle);
            bytecode_emit_switch(bc->builder, value_reg, bc_cases, default_or_post_block);

            bytecode_append_block(bc->builder, cfn, post_switch_block);
            bytecode_set_insert_point(bc->builder, cfn, post_switch_block);

            temp_array_destroy(&blocks);

            break;
        }

        case AST_Statement_Kind::SWITCH_CASE: assert(false); break;

        case AST_Statement_Kind::FALLTROUGH: {
            auto switch_info = stack_top(&bc->switch_case_stack);

            Bytecode_Block_Handle target_block = -1;

            if (switch_info.current_case_index >= switch_info.switch_stmt->switch_stmt.cases.count - 1) {
                // In the last block
                target_block = switch_info.post_block;
            } else {
                auto target_block_index = switch_info.current_case_index + 1;
                assert(target_block_index >= 0 && target_block_index < switch_info.case_blocks.count);
                target_block = switch_info.case_blocks[target_block_index];
            }

            assert(target_block != -1);

            bytecode_emit_jmp(bc->builder, target_block);
            break;
        }

        case AST_Statement_Kind::DEFER: {
            stack_push(&bc->defer_stack, stmt);
            break;
        }

        case AST_Statement_Kind::RETURN: {

            auto defer_count = stack_count(&bc->defer_stack);

            if (defer_count > 0) {
                Array_Ref<AST_Statement *> defers(bc->defer_stack.buffer, defer_count);

                for (s64 i = defers.count - 1; i >= 0; i -= 1) {
                    auto defer_stmt = defers[i];
                    assert(defer_stmt->kind == AST_Statement_Kind::DEFER);
                    ast_stmt_to_bytecode(bc, defer_stmt->defer_stmt.stmt);
                }
            }

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
                Bytecode_Register value_reg = ast_expr_to_bytecode(bc, stmt->print_expr.expressions[i]);
                bytecode_emit_print(bc->builder, value_reg);
            }

            if (stmt->print_expr.newline) {
                auto newline_value_reg = bytecode_string_literal(bc->builder, "\n");
                bytecode_emit_print(bc->builder, newline_value_reg);
            }

            break;
        }
    }

    return true;
}

Bytecode_Register ast_lvalue_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr)
{
    assert(bc);
    assert(expr);
    assert(EXPR_IS_LVALUE(expr))

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
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
                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, ident_sym->decl, &alloc_reg);
                    assert(found);

                    return alloc_reg;
                }

                case AST_Declaration_Kind::FIELD: assert(false); break;
                case AST_Declaration_Kind::FUNCTION: assert(false); break;
                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
                case AST_Declaration_Kind::ENUM_MEMBER: assert(false); break;
                case AST_Declaration_Kind::ENUM: assert(false); break;
                case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
                case AST_Declaration_Kind::IMPORT_DIRECTIVE: assert(false); break;
            }
        }

        case AST_Expression_Kind::MEMBER: {
            assert(expr->member.index_in_parent >= 0);
            Bytecode_Register base_reg;
            if (expr->member.base->resolved_type->kind == Type_Kind::POINTER) {
                base_reg = ast_expr_to_bytecode(bc, expr->member.base);
            } else {
                base_reg = ast_lvalue_to_bytecode(bc, expr->member.base);
            }
            return bytecode_emit_aggregate_offset_pointer(bc->builder, base_reg, expr->member.index_in_parent);
        }

        case AST_Expression_Kind::INDEX: {
            Bytecode_Register index_reg = ast_expr_to_bytecode(bc, expr->index.index);
            Bytecode_Register base_reg = ast_lvalue_to_bytecode(bc, expr->index.base);

            if (base_reg.type->kind == Type_Kind::STATIC_ARRAY) {

                return bytecode_emit_array_offset_pointer(bc->builder, base_reg, index_reg);

            } else if (base_reg.type->kind == Type_Kind::SLICE ||
                       (base_reg.type->kind == Type_Kind::POINTER && base_reg.type->pointer.base->kind == Type_Kind::SLICE)) {

                Bytecode_Register ptr_reg = bytecode_emit_aggregate_offset_pointer(bc->builder, base_reg, 0);
                ptr_reg = bytecode_emit_load_pointer(bc->builder, ptr_reg);

                return bytecode_emit_ptr_offset_pointer(bc->builder, ptr_reg, index_reg);

            } else {
                assert(base_reg.type == get_string_type(bc->context));
                Bytecode_Register data_reg = bytecode_emit_aggregate_offset_pointer(bc->builder, base_reg, 0);
                data_reg = bytecode_emit_load_pointer(bc->builder, data_reg);
                return bytecode_emit_ptr_offset_pointer(bc->builder, data_reg, index_reg);
            }
        }

        case AST_Expression_Kind::CALL: assert(false); break;

        case AST_Expression_Kind::UNARY: {
            if (expr->unary.op == AST_Unary_Operator::DEREF) {

                Bytecode_Register result = ast_lvalue_to_bytecode(bc, expr->unary.operand);

                if (result.kind == Bytecode_Register_Kind::ALLOC) {

                    result = bytecode_emit_load_alloc(bc->builder, result);
                }
                assert(result.type == expr->resolved_type->pointer_to);
                return result;

            } else {

                assert(expr->unary.op == AST_Unary_Operator::ADDRESS_OF);

                // This is only used by lvalue deref at the time of writing...
                Bytecode_Register result = ast_lvalue_to_bytecode(bc, expr->unary.operand);
                if (result.kind == Bytecode_Register_Kind::ALLOC) {

                    result = bytecode_emit_address_of(bc->builder, result);
                }
                return result;
            }
        }

        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::RANGE: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;
        case AST_Expression_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;
        case AST_Expression_Kind::TYPE_INFO: assert(false); break;
        case AST_Expression_Kind::TYPE: assert(false); break;
    }

    assert(false);
    return {};
}

Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type * enforce_type/*=nullptr*/)
{
    if (enforce_type) assert(!TYPE_IS_SLICE_STRUCT(enforce_type));

    assert(bc);
    assert(expr);

    if (expr->kind != AST_Expression_Kind::INTEGER_LITERAL &&
        expr->kind != AST_Expression_Kind::REAL_LITERAL &&
        expr->kind != AST_Expression_Kind::STRING_LITERAL &&
        expr->kind != AST_Expression_Kind::CHAR_LITERAL &&
        expr->kind != AST_Expression_Kind::NULL_LITERAL &&
        expr->kind != AST_Expression_Kind::RUN_DIRECTIVE &&
        !(expr->kind == AST_Expression_Kind::UNARY && expr->unary.op == AST_Unary_Operator::ADDRESS_OF) &&
        EXPR_IS_CONST(expr)) {

        assert_msg(expr->resolved_type->kind == Type_Kind::INTEGER ||
                   expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER ||
                   expr->resolved_type->kind == Type_Kind::FLOAT ||
                   expr->resolved_type->kind == Type_Kind::BOOLEAN ||
                   (expr->resolved_type->kind == Type_Kind::STRUCTURE && (expr->kind == AST_Expression_Kind::COMPOUND || expr->kind == AST_Expression_Kind::IDENTIFIER)) ||
                   (expr->resolved_type->kind == Type_Kind::STATIC_ARRAY && expr->kind == AST_Expression_Kind::COMPOUND || expr->kind == AST_Expression_Kind::IDENTIFIER) ||
                   expr->resolved_type->kind == Type_Kind::ENUM,
                   "Constant expression substition not supported for this type");

        return ast_const_expr_to_bytecode(bc, expr, enforce_type);
    }

    Bytecode_Register result = { .kind = Bytecode_Register_Kind::INVALID };

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            Type *literal_type = expr->resolved_type;
            if (literal_type->kind == Type_Kind::UNSIZED_INTEGER) {
                literal_type = &builtin_type_s64;
            }

            if (literal_type->kind == Type_Kind::FLOAT) {
                s64 value = expr->integer_literal.value.s64;
                result =  bytecode_real_literal(bc->builder, literal_type, { .r32 = (float)value, .r64 = (double)value });
                break;
            }

            if (literal_type->kind == Type_Kind::BOOLEAN) {
                u64 value = expr->integer_literal.value.u64;
                result =  bytecode_boolean_literal(bc->builder, literal_type, value != 0);
                break;
            }

            assert(literal_type->kind == Type_Kind::INTEGER);
            result = bytecode_integer_literal(bc->builder, literal_type, expr->integer_literal.value);
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: {
            Type *literal_type = expr->resolved_type;
            assert(literal_type->kind == Type_Kind::FLOAT);
            result = bytecode_real_literal(bc->builder, literal_type, expr->real_literal.value);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            result = bytecode_string_literal(bc->builder, expr->string_literal.atom);
            break;
        }

        case AST_Expression_Kind::CHAR_LITERAL: {
            result = bytecode_integer_literal(bc->builder, &builtin_type_u8, expr->character_literal);
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            result = bytecode_pointer_literal(bc->builder, expr->resolved_type, nullptr);
            break;
        }

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

                        result = bytecode_emit_load_global(bc->builder, global_handle);

                    } else {
                        Bytecode_Register alloc_reg;
                        bool found = hash_table_find(&bc->allocations, ident_decl, &alloc_reg);
                        assert(found);

                        result = bytecode_emit_load_alloc(bc->builder, alloc_reg);
                    }

                    break;
                }

                case AST_Declaration_Kind::CONSTANT_VARIABLE: {
                    assert(ident_decl->variable.value);
                    result = ast_expr_to_bytecode(bc, ident_decl->variable.value);
                    break;
                }

                case Zodiac::AST_Declaration_Kind::PARAMETER: {
                    assert(ident_decl->parameter.resolved_type);

                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, ident_decl, &alloc_reg);
                    assert(found);

                    result = bytecode_emit_load_alloc(bc->builder, alloc_reg);
                    break;
                }

                case Zodiac::AST_Declaration_Kind::FIELD: assert(false); break;

                case AST_Declaration_Kind::FUNCTION: {
                    Bytecode_Function_Handle fn_handle;
                    bool found = hash_table_find(&bc->functions, ident_decl, &fn_handle);
                    assert(found);

                    result = bytecode_emit_addrof_func(bc->builder, fn_handle);
                    assert(result.type->kind == Type_Kind::FUNCTION);
                    break;
                }

                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::ENUM_MEMBER: assert(false); break;
                case AST_Declaration_Kind::ENUM: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
                case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
                case AST_Declaration_Kind::IMPORT_DIRECTIVE: assert(false); break;
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: {
            auto base_type = expr->member.base->resolved_type;

            if (base_type->kind == Type_Kind::STATIC_ARRAY) {
                auto array_type = base_type;
                assert(expr->resolved_type == &builtin_type_s64);
                return bytecode_integer_literal(bc->builder, expr->resolved_type, array_type->static_array.count);
            } else if (base_type->kind == Type_Kind::ENUM) {

                s64 value_index = expr->member.index_in_parent;
                assert(value_index >= 0 && value_index < base_type->enumeration.members.count);
                auto res = bytecode_integer_literal(bc->builder, base_type->enumeration.integer_type, base_type->enumeration.members[value_index].value);
                res.type = base_type;
                result = res;
                break;
            }

            assert(expr->member.index_in_parent >= 0);
            Bytecode_Register base_reg;
            if (base_type->kind == Type_Kind::POINTER) {
                base_reg = ast_expr_to_bytecode(bc, expr->member.base);
            } else {
                base_reg = ast_lvalue_to_bytecode(bc, expr->member.base);
            }
            Bytecode_Register addr_reg = bytecode_emit_aggregate_offset_pointer(bc->builder, base_reg, expr->member.index_in_parent);
            result = bytecode_emit_load_pointer(bc->builder, addr_reg);
            break;
        }

        case AST_Expression_Kind::INDEX: {
            Bytecode_Register addr_reg = ast_lvalue_to_bytecode(bc, expr);
            assert(addr_reg.kind == Bytecode_Register_Kind::TEMPORARY);
            assert(addr_reg.type->kind == Type_Kind::POINTER);
            result = bytecode_emit_load_pointer(bc->builder, addr_reg);
            break;
        }

        case AST_Expression_Kind::CALL: {

            Type *fn_type = nullptr;
            bool ptr_call = false;
            Bytecode_Function_Handle fn_handle = -1;

            AST_Expression *base = expr->call.base;
            if (base->kind == AST_Expression_Kind::IDENTIFIER) {

                Symbol *sym = scope_get_symbol(base->identifier.scope, base->identifier.name);
                assert(sym);

                if (sym->kind == Symbol_Kind::FUNC) {
                    assert(sym->decl && sym->decl->kind == AST_Declaration_Kind::FUNCTION);

                    bool found = hash_table_find(&bc->functions, sym->decl, &fn_handle);
                    assert(found);

                    auto fn = bc->builder->functions[fn_handle];
                    fn_type = fn.type;

                } else {
                    assert(sym->kind == Symbol_Kind::VAR ||
                           sym->kind == Symbol_Kind::CONST ||
                           sym->kind == Symbol_Kind::PARAM);

                    if (sym->kind == Symbol_Kind::CONST) {
                        auto init_expr = sym->decl->variable.value;
                        assert(init_expr->kind == AST_Expression_Kind::IDENTIFIER);

                        auto val_sym = scope_get_symbol(init_expr->identifier.scope, init_expr->identifier);
                        assert(val_sym->decl && val_sym->decl->kind == AST_Declaration_Kind::FUNCTION);

                        bool found = hash_table_find(&bc->functions, val_sym->decl, &fn_handle);
                        assert(found);

                    } else {
                        ptr_call = true;
                    }

                    assert(sym->decl->variable.resolved_type->kind == Type_Kind::FUNCTION);
                    fn_type = sym->decl->variable.resolved_type;
                }
            } else {
                assert(base->resolved_type->kind == Type_Kind::FUNCTION);
                fn_type = base->resolved_type;
                ptr_call = true;
            }

            assert(fn_type);

            auto arg_count = expr->call.args.count;
            assert(ptr_call || fn_handle >= 0);

            auto any_type = get_any_type(bc->context);

            for (s64 i = 0; i < expr->call.args.count; i++) {

                auto arg_expr = expr->call.args[i];
                Bytecode_Register arg_reg;

                if (fn_type->function.parameter_types[i] == any_type && arg_expr->resolved_type != any_type) {
                    arg_reg = emit_any(bc, arg_expr);
                } else if (arg_expr->flags & AST_EXPR_FLAG_SLICE_ARRAY) {

                    Type *array_type = arg_expr->resolved_type;
                    assert(array_type->kind == Type_Kind::STATIC_ARRAY);

                    Bytecode_Register array_alloc;
                    if (arg_expr->kind == AST_Expression_Kind::COMPOUND || EXPR_IS_CONST(arg_expr)) {
                        bool found = hash_table_find(&bc->implicit_lvalues, arg_expr, &array_alloc);
                        assert(found);
                    } else if (EXPR_IS_LVALUE(arg_expr)){
                        array_alloc = ast_lvalue_to_bytecode(bc, arg_expr);
                    } else {
                        assert(false);
                    }

                    Bytecode_Register ptr_reg = bytecode_emit_array_offset_pointer(bc->builder, array_alloc, 0);
                    Bytecode_Register length_reg = bytecode_integer_literal(bc->builder, &builtin_type_s64, array_type->static_array.count);

                    auto slice_type = fn_type->function.parameter_types[i];
                    assert(slice_type->kind == Type_Kind::SLICE);

                    arg_reg = bytecode_emit_insert_value(bc->builder, {}, ptr_reg, slice_type->slice.struct_type, 0);
                    arg_reg = bytecode_emit_insert_value(bc->builder, arg_reg, length_reg, slice_type->slice.struct_type, 1);

                } else {
                    arg_reg = ast_expr_to_bytecode(bc, arg_expr, fn_type->function.parameter_types[i]);
                }

                bytecode_emit_push_arg(bc->builder, arg_reg);
            }

            if (ptr_call) {
                auto fn_ptr_reg = ast_expr_to_bytecode(bc, base);
                result = bytecode_emit_call_pointer(bc->builder, fn_ptr_reg, arg_count);
            } else {
                result = bytecode_emit_call(bc->builder, fn_handle, arg_count);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: {

            switch (expr->unary.op) {

                case AST_Unary_Operator::INVALID: assert(false); break;

                case AST_Unary_Operator::PLUS:
                case AST_Unary_Operator::MINUS: {
                    assert(false);
                    break;
                }

                case AST_Unary_Operator::ADDRESS_OF: {
                    if (EXPR_IS_CONST(expr)) {

                        auto operand = expr->unary.operand;

                        Bytecode_Register alloc_reg;
                        bool found = hash_table_find(&bc->implicit_lvalues, operand, &alloc_reg);
                        assert(found);

                        result = bytecode_emit_address_of(bc->builder, alloc_reg);

                    } else {
                        Bytecode_Register lvalue_reg = ast_lvalue_to_bytecode(bc, expr->unary.operand);

                        if (lvalue_reg.kind == Bytecode_Register_Kind::ALLOC ||
                            lvalue_reg.kind == Bytecode_Register_Kind::GLOBAL) {

                            result = bytecode_emit_address_of(bc->builder, lvalue_reg);

                        } else {
                            assert(lvalue_reg.kind == Bytecode_Register_Kind::TEMPORARY);
                            result = lvalue_reg;
                        }
                    }
                    break;
                }

                case AST_Unary_Operator::DEREF: {
                    auto operand = expr->unary.operand;
                    assert(operand->resolved_type->kind == Type_Kind::POINTER);

                    Bytecode_Register ptr_reg = ast_expr_to_bytecode(bc, operand);
                    result = bytecode_emit_load_pointer(bc->builder, ptr_reg);
                    break;
                }

                case AST_Unary_Operator::NOT: {
                    auto operand = expr->unary.operand;

                    auto op_reg = ast_expr_to_bytecode(bc, operand);

                    if (operand->resolved_type->kind == Type_Kind::BOOLEAN) {
                        result = bytecode_emit_xor(bc->builder, op_reg, bytecode_boolean_literal(bc->builder, &builtin_type_bool, true));

                    } else if (operand->resolved_type->kind == Type_Kind::POINTER) {

                        assert(builtin_type_s64.bit_size == pointer_size);
                        Bytecode_Register as_int = bytecode_emit_bitcast(bc->builder, &builtin_type_s64, op_reg);
                        result = bytecode_emit_eq(bc->builder, as_int, bytecode_zero_value(bc->builder, &builtin_type_s64));

                    } else {
                        assert(false);
                    }
                    break;
                }
            }

            break;
        }

        case AST_Expression_Kind::BINARY: {

            auto lhs = expr->binary.lhs;
            auto rhs = expr->binary.rhs;

            assert(lhs->resolved_type == rhs->resolved_type);

            if (lhs->resolved_type->kind == Type_Kind::POINTER) {
                assert(expr->binary.op == AST_Binary_Operator::EQ ||
                       expr->binary.op == AST_Binary_Operator::NEQ)
            }

            Bytecode_Register lhs_reg = ast_expr_to_bytecode(bc, lhs);
            Bytecode_Register rhs_reg = ast_expr_to_bytecode(bc, rhs);

            switch (expr->binary.op) {
                case AST_Binary_Operator::INVALID: assert(false); break;

                case AST_Binary_Operator::ADD: {
                    result = bytecode_emit_add(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::SUB: {
                    result = bytecode_emit_sub(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::MUL: {
                    result = bytecode_emit_mul(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::DIV: {
                    result = bytecode_emit_div(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::MOD: {
                    result = bytecode_emit_mod(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::EQ: {
                    result = bytecode_emit_eq(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::NEQ: {
                    result = bytecode_emit_neq(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::LT: {
                    result = bytecode_emit_lt(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::GT: {
                    result = bytecode_emit_gt(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::LTEQ: {
                    result = bytecode_emit_lteq(bc->builder, lhs_reg, rhs_reg);
                    break;
                }

                case AST_Binary_Operator::GTEQ: {
                    result = bytecode_emit_gteq(bc->builder, lhs_reg, rhs_reg);
                    break;
                }
            }
            break;
        }

        case AST_Expression_Kind::RANGE: assert(false); break;

        case AST_Expression_Kind::CAST: {
            debug_assert(expr->resolved_type);

            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, expr->cast.value);
            result = bytecode_emit_cast(bc->builder, expr->resolved_type, value_reg);
            break;
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: {

            assert(EXPR_IS_TYPED(expr));
            assert(expr->directive.directive->kind == AST_Directive_Kind::RUN);
            assert(expr->directive.directive->run.kind == AST_Run_Directive_Kind::EXPR);

            auto directive = &expr->directive;

            assert(directive->generated_expression);

            Bytecode_Register value_reg;
            bool found = hash_table_find(&bc->run_results, directive->directive, &value_reg);
            assert(found);

            result = value_reg;
            break;
        }

        case AST_Expression_Kind::COMPOUND: {
            result = ast_compound_expr_to_bytecode(bc, expr);
            break;
        }

        case AST_Expression_Kind::TYPE_INFO: {

            Type *target_type = expr->directive.directive->type_info.type_spec->resolved_type;
            result = emit_type_info(bc, target_type);
            break;
        }

        case Zodiac::AST_Expression_Kind::TYPE: {
            assert(false);
            break;
        }
    }

    assert(result.kind != Bytecode_Register_Kind::INVALID ||
           expr->kind == AST_Expression_Kind::CALL && expr->resolved_type->kind == Type_Kind::VOID);

    auto check_type = expr->resolved_type;
    if (check_type->kind == Type_Kind::UNSIZED_INTEGER) {
        check_type = result.type;
    }

    if (enforce_type && check_type != enforce_type) {

        if (enforce_type->kind == Type_Kind::BOOLEAN) {
            assert(check_type->kind == Type_Kind::INTEGER);

            if (EXPR_IS_CONST(expr)) {
                u64 value = expr->integer_literal.value.u64;
                result = bytecode_boolean_literal(bc->builder, enforce_type, value != 0);
            } else {
                result = bytecode_emit_neq(bc->builder, result, bytecode_integer_literal(bc->builder, expr->resolved_type, 0));
            }
        } else {
            assert(false);
        }
    }
    return result;
}

Bytecode_Register ast_compound_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *compound_expr) {
    debug_assert(bc && compound_expr);

    assert(compound_expr->resolved_type);
    auto type = compound_expr->resolved_type;
    bool aggregate = type->flags & TYPE_FLAG_AGGREGATE;
    assert(aggregate || type->kind == Type_Kind::STATIC_ARRAY);

    Bytecode_Register result = { .kind = Bytecode_Register_Kind::INVALID };

    if (aggregate) {
        assert(type->kind == Type_Kind::STRUCTURE);
    }

    for (s64 i = 0; i < compound_expr->compound.expressions.count; i++) {
        auto member_expr = compound_expr->compound.expressions[i];
        Type *elem_type = aggregate ? type->structure.member_types[i] : type->static_array.element_type;
        Bytecode_Register member_reg = ast_expr_to_bytecode(bc, member_expr, elem_type);

        if (aggregate) {
            result = bytecode_emit_insert_value(bc->builder, result, member_reg, type, i);
        } else {
            result = bytecode_emit_insert_element(bc->builder, result, member_reg, type, i);
        }
    }

    assert(result.kind != Bytecode_Register_Kind::INVALID); // Probably means the compound literal is empty

    return result;
}

Bytecode_Register ast_const_lvalue_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr)
{
    assert(EXPR_IS_CONST(expr));

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(expr->identifier.scope);
            Symbol *ident_sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
            assert(ident_sym);
            assert(ident_sym->decl);

            switch (ident_sym->decl->kind) {

                case AST_Declaration_Kind::INVALID: assert(false); break;
                case AST_Declaration_Kind::VARIABLE: assert(false); break;

                case AST_Declaration_Kind::CONSTANT_VARIABLE: {
                    Bytecode_Global_Handle glob_handle;
                    bool found = hash_table_find(&bc->globals, ident_sym->decl, &glob_handle);
                    assert(found);

                    debug_assert(glob_handle < bc->builder->globals.count);
                    auto global = bc->builder->globals[glob_handle];
                    assert(global.constant);

                    Bytecode_Register global_reg;
                    bool reg_found = hash_table_find(&bc->builder->global_registers, global.atom, &global_reg);
                    assert(reg_found);

                    debug_assert(global_reg.kind == Bytecode_Register_Kind::GLOBAL);
                    return global_reg;
                }

                case AST_Declaration_Kind::PARAMETER: {
                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, ident_sym->decl, &alloc_reg);
                    assert(found);

                    return alloc_reg;
                }

                case AST_Declaration_Kind::FIELD: assert(false); break;
                case AST_Declaration_Kind::FUNCTION: assert(false); break;
                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
                case AST_Declaration_Kind::ENUM_MEMBER: assert(false); break;
                case AST_Declaration_Kind::ENUM: assert(false); break;
                case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
                case AST_Declaration_Kind::IMPORT_DIRECTIVE: assert(false); break;
            }

        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::RANGE: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;
        case AST_Expression_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;
        case AST_Expression_Kind::TYPE_INFO: assert(false); break;
        case AST_Expression_Kind::TYPE: assert(false); break;
    }
}

Bytecode_Register ast_const_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type *enforce_type/*=nullptr*/)
{
    debug_assert(bc);
    assert(EXPR_IS_CONST(expr));

    assert(expr->resolved_type);
    auto type = expr->resolved_type;

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            if (enforce_type && enforce_type->kind == Type_Kind::FLOAT) {
                auto iv = expr->integer_literal.value;
                return bytecode_real_literal(bc->builder, enforce_type, { .r32 = (float)iv.s64, .r64 = (double)iv.s64});
            } else if (enforce_type && enforce_type->kind == Type_Kind::BOOLEAN) {
                assert(false);
            } else {
                assert(!enforce_type || enforce_type->kind == Type_Kind::INTEGER);
            }
            return bytecode_integer_literal(bc->builder, type, expr->integer_literal.value);
        }

        case AST_Expression_Kind::REAL_LITERAL: {
            return bytecode_real_literal(bc->builder, expr->resolved_type, expr->real_literal.value);
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            return bytecode_string_literal(bc->builder, expr->string_literal.atom);
        }

        case AST_Expression_Kind::CHAR_LITERAL: {
            assert(false);
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            return bytecode_pointer_literal(bc->builder, expr->resolved_type, nullptr);
        }

        case AST_Expression_Kind::BOOL_LITERAL: {
            return bytecode_boolean_literal(bc->builder, type, expr->bool_literal);
        }

        case AST_Expression_Kind::IDENTIFIER: {

            if (type->kind == Type_Kind::UNSIZED_INTEGER) {
                assert(enforce_type);
                assert(enforce_type->kind != Type_Kind::UNSIZED_INTEGER);
                type = enforce_type;
            }

            switch (type->kind) {

                case Type_Kind::INVALID: assert(false); break;
                case Type_Kind::VOID: assert(false); break;
                case Type_Kind::UNSIZED_INTEGER: assert(false); break;

                case Type_Kind::INTEGER: {
                    auto result = resolve_constant_integer_expr(expr, type);
                    assert(result.kind == Constant_Resolve_Result_Kind::OK);
                    assert(result.type == type);
                    auto result_value = result.integer;
                    return bytecode_integer_literal(bc->builder, type, result_value);
                }

                case Type_Kind::FLOAT: {
                    auto result = resolve_constant_real_expr(expr);
                    assert(result.kind == Constant_Resolve_Result_Kind::OK);
                    assert(result.type == type || result.type->kind == Type_Kind::UNSIZED_INTEGER);
                    auto result_value = result.real;
                    return bytecode_real_literal(bc->builder, type, result_value);
                }

                case Type_Kind::BOOLEAN: {
                    auto result = resolve_constant_bool_expr(expr);
                    assert(result.kind == Constant_Resolve_Result_Kind::OK);
                    assert(result.type == type);
                    return bytecode_boolean_literal(bc->builder, type, result.boolean);
                }

                case Type_Kind::POINTER: assert(false);

                case Type_Kind::STRUCTURE:
                case Type_Kind::STATIC_ARRAY: {
                    auto sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
                    assert(sym && sym->decl);
                    auto decl = sym->decl;
                    assert(decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);
                    assert(decl->variable.resolved_type == type);
                    auto init_expr = decl->variable.value;
                    assert(init_expr->resolved_type == type);

                    if (init_expr->kind == AST_Expression_Kind::RUN_DIRECTIVE) {
                        assert(init_expr->directive.generated_expression);
                        init_expr = init_expr->directive.generated_expression;
                    }
                    assert(init_expr->kind == AST_Expression_Kind::COMPOUND);

                    return ast_const_compound_expr_to_bytecode(bc, init_expr);
                }

                case Type_Kind::SLICE: {
                    auto sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
                    assert(sym && sym->decl);
                    auto decl = sym->decl;
                    assert(decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);
                    assert(decl->variable.resolved_type == type);

                    Bytecode_Global_Handle glob_handle;
                    bool found = hash_table_find(&bc->globals, sym->decl, &glob_handle);
                    assert(found);

                    auto glob = bc->builder->globals[glob_handle];
                    return glob.initial_value;
                }

                case Type_Kind::ENUM: assert(false); break;


                case Type_Kind::FUNCTION: {
                    auto sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
                    assert(sym && sym->decl);

                    Bytecode_Register result;

                    if (sym->decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE) {

                        result = ast_const_expr_to_bytecode(bc, sym->decl->variable.value);

                    } else if (sym->decl->kind == AST_Declaration_Kind::FUNCTION) {

                        Bytecode_Function_Handle fn_handle;
                        bool found = hash_table_find(&bc->functions, sym->decl, &fn_handle);
                        assert(found);


                        Bytecode_Function *fn = &bc->builder->functions[fn_handle];

                        if (!(fn->flags & BC_FUNCTION_FLAG_FOREIGN) && !fn->ffi_handle) {

                            FFI_Function_User_Data func_data = { .interp = bc->context->interp, .handle = fn_handle };
                            FFI_Handle ffi_handle = ffi_create_callback(&bc->context->interp->ffi, func_data, fn->type);
                            assert(ffi_handle);
                            fn->ffi_handle = ffi_handle;
                        }

                        assert(fn->ffi_handle);

                        result = bytecode_register_create(bc->builder, Bytecode_Register_Kind::FUNCTION, type, BC_REGISTER_FLAG_LITERAL | BC_REGISTER_FLAG_CONSTANT);
                        result.value.function_handle = fn_handle;
                    }

                    assert(result.kind != Bytecode_Register_Kind::INVALID);

                    return result;
                }
            }

            assert(false); // should have returned
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            assert(expr->resolved_type->kind == Type_Kind::ENUM);
            auto enum_type = expr->resolved_type;

            Constant_Resolve_Result result = resolve_constant_integer_expr(expr);
            assert(result.kind == Constant_Resolve_Result_Kind::OK);
            assert(result.type == enum_type);
            auto result_reg = bytecode_integer_literal(bc->builder, enum_type->enumeration.integer_type, result.integer.s64);
            result_reg.type = enum_type;
            return result_reg;
        }

        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;

        case AST_Expression_Kind::UNARY: {

            switch (expr->unary.op) {

                case AST_Unary_Operator::INVALID: assert(false); break;

                case AST_Unary_Operator::PLUS:
                case AST_Unary_Operator::MINUS: {
                    auto result = resolve_constant_integer_expr(expr, type);
                    assert(result.kind == Constant_Resolve_Result_Kind::OK);
                    assert(result.type == type);

                    if (enforce_type && enforce_type->kind == Type_Kind::BOOLEAN) {
                        return bytecode_boolean_literal(bc->builder, enforce_type, result.integer.u64 != 0);
                    } else {
                        assert(!enforce_type || enforce_type == type);
                        return bytecode_integer_literal(bc->builder, type, result.integer);
                    }
                }

                case AST_Unary_Operator::ADDRESS_OF: assert(false); break;
                case AST_Unary_Operator::DEREF: assert(false); break;
                case AST_Unary_Operator::NOT: assert(false); break;
            }


        }

        case AST_Expression_Kind::BINARY: {
            bool is_cmp = is_binary_cmp_op(expr->binary.op);

            assert(type->kind == Type_Kind::INTEGER ||
                   (type->kind == Type_Kind::BOOLEAN && is_cmp));

            auto result = resolve_constant_integer_binary_expr(expr, type);
            assert(result.kind == Constant_Resolve_Result_Kind::OK);
            if (is_cmp) {
                assert(result.type == &builtin_type_bool);
                return bytecode_boolean_literal(bc->builder, result.type, result.integer.u64);
            } else {
                assert(result.type == type);
                return bytecode_integer_literal(bc->builder, result.type, result.integer);
            }

            break;
        }

        case AST_Expression_Kind::RANGE: assert(false); break;

        case AST_Expression_Kind::CAST: {

            auto value_expr = expr->cast.value;

            if (type->kind == Type_Kind::INTEGER) {

                assert(value_expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER);

                auto result = resolve_constant_integer_expr(value_expr, type);
                assert(result.kind == Constant_Resolve_Result_Kind::OK);
                assert(result.type == type);
                return bytecode_integer_literal(bc->builder, type, result.integer);

            } else if (type->kind == Type_Kind::POINTER) {

                assert(value_expr->resolved_type == &builtin_type_u64);

                auto result = resolve_constant_integer_expr(value_expr, value_expr->resolved_type);
                assert(result.kind == Constant_Resolve_Result_Kind::OK);
                assert(result.type == value_expr->resolved_type);
                return bytecode_pointer_literal(bc->builder, expr->resolved_type, result.pointer);

            } else if (type->kind == Type_Kind::BOOLEAN) {

                assert(value_expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER);

                auto result = resolve_constant_integer_expr(value_expr, &builtin_type_u64);
                assert(result.kind == Constant_Resolve_Result_Kind::OK);
                assert(result.type == &builtin_type_u64);

                return bytecode_boolean_literal(bc->builder, type, result.integer.u64 != 0);
            } else {
                assert(false);
            }
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: assert(false); break;

        case AST_Expression_Kind::COMPOUND: {
            assert((type->flags & TYPE_FLAG_AGGREGATE) || type->kind == Type_Kind::STATIC_ARRAY);

            if (type == get_string_type(bc->context)) {
                assert(expr->compound.expressions.count == 2);
                auto ptr_expr = expr->compound.expressions[0];
                auto length_expr = expr->compound.expressions[1];

                assert(length_expr->kind == AST_Expression_Kind::INTEGER_LITERAL);

                auto result = resolve_constant_pointer_expression(ptr_expr);
                assert(result.kind == Constant_Resolve_Result_Kind::OK);
                assert(result.type == ptr_expr->resolved_type);
                return bytecode_string_literal(bc->builder, String_Ref((char *)result.pointer, length_expr->integer_literal.value.s64));
            }
            return ast_const_compound_expr_to_bytecode(bc, expr);
        }

        case AST_Expression_Kind::TYPE_INFO: assert(false); break;
        case AST_Expression_Kind::TYPE: assert(false); break;
    }

    assert(false);
    return {};
}

Bytecode_Register ast_const_compound_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *compound_expr) {
    debug_assert(bc && compound_expr);
    assert(EXPR_IS_CONST(compound_expr));

    assert(compound_expr->resolved_type);
    auto type = compound_expr->resolved_type;
    bool aggregate = type->flags & TYPE_FLAG_AGGREGATE;
    assert(aggregate || type->kind == Type_Kind::STATIC_ARRAY);

    if (aggregate) {
        assert(type->kind == Type_Kind::STRUCTURE);
    }

    Dynamic_Array<Bytecode_Register> values;
    dynamic_array_create<Bytecode_Register>(bc->allocator, &values, compound_expr->compound.expressions.count);


    for (s64 i = 0; i < compound_expr->compound.expressions.count; i++) {
        auto value_expr = compound_expr->compound.expressions[i];
        assert(EXPR_IS_CONST(value_expr));

        Type *elem_type = aggregate ? type->structure.member_types[i] : type->static_array.element_type;
        Bytecode_Register value_reg = ast_const_expr_to_bytecode(bc, value_expr, elem_type);
        dynamic_array_append(&values, value_reg);
    }

    if (aggregate) {
        return bytecode_aggregate_literal(bc->builder, values, type);
    } else {
        return bytecode_array_literal(bc->builder, values, type);
    }

}

void assignment_to_bytecode(Bytecode_Converter *bc, AST_Expression *value_expr, Bytecode_Register lvalue_reg)
{

    Type *lvalue_type = lvalue_reg.type;

    if (lvalue_reg.kind == Bytecode_Register_Kind::TEMPORARY) {
        assert(lvalue_reg.type->kind == Type_Kind::POINTER);
        lvalue_type = lvalue_type->pointer.base;
    }

    Bytecode_Register value_reg;

    auto any_type = get_any_type(bc->context);
    assert(any_type);

    if (lvalue_type == any_type && value_expr->resolved_type != any_type) {
        assert(EXPR_IS_LVALUE(value_expr));

        value_reg = emit_any(bc, value_expr);

    } else if (value_expr->resolved_type->kind == Type_Kind::STATIC_ARRAY && lvalue_type->kind == Type_Kind::SLICE) {

        Bytecode_Register array_reg;

        if (EXPR_IS_LVALUE(value_expr)) {
            array_reg = ast_lvalue_to_bytecode(bc, value_expr);
        } else {
            bool found = hash_table_find(&bc->implicit_lvalues, value_expr, &array_reg);
            assert(found);
        }

        Bytecode_Register length_reg = bytecode_integer_literal(bc->builder, &builtin_type_s64, array_reg.type->static_array.count);

        Bytecode_Register members[2] = { array_reg, length_reg };
        value_reg = bytecode_aggregate_literal(bc->builder, members, lvalue_type->slice.struct_type);
        value_reg.type = lvalue_type;

    } else {
        value_reg = ast_expr_to_bytecode(bc, value_expr, lvalue_type);
    }

    assert(value_reg.type == lvalue_type);

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
}

Bytecode_Register emit_type_info(Bytecode_Converter *bc, Type *target_type)
{
    assert(target_type);
    assert(bc->builder->insert_fn_index >= 0);

    if (target_type->info_index == -1) {
        add_type_info(bc->context, target_type);
    }

    // TODO: We don't need to do this every time!
    Bytecode_Register global_reg;
    bool found = hash_table_find(&bc->builder->global_registers, atom_get(&bc->context->atoms, "_type_info_pointers"), &global_reg);
    assert(found);

    auto arr_ptr = bytecode_emit_aggregate_offset_pointer(bc->builder, global_reg, 0);
    auto arr = bytecode_emit_load(bc->builder, arr_ptr);
    auto elem_ptr = bytecode_emit_ptr_offset_pointer(bc->builder, arr, target_type->info_index);

    return bytecode_emit_load(bc->builder, elem_ptr);
}

Bytecode_Register emit_any(Bytecode_Converter *bc, AST_Expression *expr)
{
    auto any_type = get_any_type(bc->context);

    auto typeinfo_reg = emit_type_info(bc, expr->resolved_type);
    auto ptr_reg = ast_lvalue_to_bytecode(bc, expr);

    if (ptr_reg.kind == Bytecode_Register_Kind::ALLOC) {
        ptr_reg = bytecode_emit_address_of(bc->builder, ptr_reg);
    }

    if (ptr_reg.type != any_type->structure.member_types[1]) {
        ptr_reg = bytecode_emit_cast(bc->builder, any_type->structure.member_types[1], ptr_reg);
    }

    auto result = bytecode_emit_insert_value(bc->builder, {}, typeinfo_reg, any_type, 0);
    result = bytecode_emit_insert_value(bc->builder, result, ptr_reg, any_type, 1);

    return result;
}

Bytecode_Function_Handle create_run_wrapper(Bytecode_Converter *bc, AST_Directive *run_directive)
{
    debug_assert(bc && run_directive);
    debug_assert(run_directive->kind == AST_Directive_Kind::RUN);

    // We might call this function when we are in the middle of emitting another function,
    //   so we cache the current insert point, and reset it before we return
    auto original_insert_fn_index = bc->builder->insert_fn_index;
    auto original_insert_block_index = bc->builder->insert_block_index;

    Atom run_wrapper_name;
    {
        auto spos = run_directive->sr.start;

        char buf[256];
        auto len = string_format(buf, "rw_%.*s:%i:%i", (int)spos.name.length, spos.name.data, spos.line, spos.index_in_line);
        bc->run_directive_count += 1;
        assert(len < 256);

        run_wrapper_name = atom_get(&bc->context->atoms, buf);
    }

    Type *return_type = &builtin_type_void;

    bool return_value = false;

    if (run_directive->run.kind == AST_Run_Directive_Kind::EXPR) {
        return_type = run_directive->run.expr->resolved_type;
        if (return_type != &builtin_type_void) return_value = true;
    }

    Type *run_wrapper_type = get_function_type(return_type, {}, &bc->context->ast_allocator);

    auto wrapper_handle = bytecode_function_create(bc->builder, run_wrapper_name, run_wrapper_type, BC_FUNCTION_FLAG_RUN_WRAPPER);
    auto entry_block = bytecode_append_block(bc->builder, wrapper_handle, "entry");
    bytecode_set_insert_point(bc->builder, wrapper_handle, entry_block);

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

    Bytecode_Validator validator = {};

    // We need to pass in all the functions, otherwise the handles won't work
    auto functions = Array_Ref(bc->builder->functions);

    if (bc->context->options.validate_bytecode) {
        bytecode_validator_init(bc->context, temp_allocator_allocator(), &validator, functions, nullptr);
        bool bytecode_valid = validate_bytecode(&validator);

        if (!bytecode_valid) {
            assert(validator.errors.count);

            if (bc->context->options.print_bytecode) {
                bytecode_print(bc->context->bytecode_builder, temp_allocator_allocator());
            }

            bytecode_validator_print_errors(&validator);
            return -1;
        }
    }

    bc->builder->insert_fn_index = original_insert_fn_index;
    bc->builder->insert_block_index = original_insert_block_index;

    return wrapper_handle;
}

Run_Wrapper_Result execute_run_wrapper(Bytecode_Converter *bc, Bytecode_Function_Handle fn_handle)
{
    File_Handle stdout_file;
    filesystem_stdout_file(&stdout_file);
    return execute_run_wrapper(bc, fn_handle, stdout_file);
}

Run_Wrapper_Result execute_run_wrapper(Bytecode_Converter *bc, Bytecode_Function_Handle fn_handle, File_Handle stdout_file)
{
    debug_assert(bc && fn_handle >= 0 && stdout_file.valid);

#ifndef NDEBUG
    debug_assert(fn_handle >= 0 && fn_handle < bc->builder->functions.count);
    auto fn = &bc->builder->functions[fn_handle];
    ZTRACE("Executing run wrapper: '%s'", fn->name.data);
#endif // NDEBUG

    auto run_prog = bytecode_get_program(bc->builder);

    auto run_interp = bc->context->interp;

    run_interp->std_out = stdout_file;
    Interpreter_Register result = interpreter_start(run_interp, run_prog, fn_handle);

    return { &dynamic_allocator, run_interp, result };
}

AST_Expression *interpreter_register_to_ast_expression(Bytecode_Converter *bc, Interpreter_Register &reg, Scope *scope, Source_Range range)
{
    debug_assert(bc);
    debug_assert(reg.type);

    auto ctx = bc->context;

    AST_Expression *result = nullptr;
    Infer_Node *infer_node = infer_node_new(bc->context, reg.type);

    bool needs_resolving = true;

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

        case Type_Kind::STRUCTURE:
        case Type_Kind::STATIC_ARRAY: {

            assert(reg.value.pointer);
            result = interpreter_memory_to_ast_expression(bc, reg.pointer, reg.type, scope, range);

            needs_resolving = false;
            assert(EXPR_IS_TYPED(result));
            break;
        }

        case Type_Kind::SLICE: assert(false); break;
        case Type_Kind::ENUM: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
    }

    assert(result);

    if (needs_resolving) {
#ifndef NDEBUG
        bool resolved_name =
#endif // NDEBUG
            name_resolve_expr(ctx, result, scope);
        debug_assert(resolved_name);

#ifndef NDEBUG
        bool resolved_type =
#endif // NDEBUG
            type_resolve_expression(ctx->resolver, result, scope, infer_node);
        debug_assert(resolved_type);
    }


    return result;
}

AST_Expression *interpreter_memory_to_ast_expression(Bytecode_Converter *bc, u8* mem, Type *type, Scope *scope, Source_Range range)
{
    debug_assert(bc && mem && type && scope);

    auto ctx = bc->context;

    AST_Expression *result = nullptr;
    Infer_Node *infer_node = infer_node_new(bc->context, type);

    switch (type->kind) {

        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::UNSIZED_INTEGER:
        case Type_Kind::INTEGER: {
            Integer_Value val;
            switch (type->bit_size) {
                default: assert_msg(false, "Unsupported integer size in 'interpreter_memory_to_ast_expression'"); break;
                case 8: val.u8 = *(u8*)mem; break;
                case 16: val.u16 = *(u16*)mem; break;
                case 32: val.u32 = *(u32*)mem; break;
                case 64: val.u64 = *(u64*)mem; break;
            }

            result = ast_integer_literal_expr_new(ctx, range, val);
            break;
        }

        case Type_Kind::FLOAT: {
            Real_Value val;
            switch (type->bit_size) {
                default: assert_msg(false, "Unsupported real size in 'interpreter_memory_to_ast_expression'"); break;
                case 32: val.r32 = *(r32*)mem; break;
                case 64: val.r64 = *(r64*)mem; break;
            }

            result = ast_real_literal_expr_new(ctx, range, val);
            break;
        }

        case Type_Kind::BOOLEAN: {
            bool val = *(bool*)mem;
            result = ast_bool_literal_expr_new(ctx, range, val);
            break;
        }

        case Type_Kind::POINTER: {
            // ZWARN("Making pointer literal from interpreter memory, this pointer will probably be invalid...");

            Integer_Value intval = { .u64 = *(u64*)mem };
            result = ast_integer_literal_expr_new(ctx, range, intval);
            name_resolve_expr(ctx, result, scope);

            auto lit_infer_node = infer_node_new(ctx, &builtin_type_u64);
            type_resolve_expression(ctx->resolver, result, scope, lit_infer_node);

            result = ast_cast_expr_new(ctx, range, type, result);
            break;
        }

        case Type_Kind::STRUCTURE: {

            u8 *cursor = mem;

            Dynamic_Array<AST_Expression *> member_exprs;
            dynamic_array_create(&bc->context->ast_allocator, &member_exprs, type->structure.member_types.count);

            for (s64 i = 0; i < type->structure.member_types.count; i++) {
                auto member_type = type->structure.member_types[i];
                assert(member_type->bit_size % 8 == 0);
                auto size = member_type->bit_size / 8;

                auto member_expr = interpreter_memory_to_ast_expression(bc, cursor, member_type, scope, range);
                cursor += size;
                dynamic_array_append(&member_exprs, member_expr);
            }

            result = ast_compound_expr_new(bc->context, range, member_exprs);
            break;
        }

        case Type_Kind::ENUM: assert(false); break;

        case Type_Kind::STATIC_ARRAY: {
            u8 *cursor = mem;

            Dynamic_Array<AST_Expression *> value_exprs;
            dynamic_array_create(&bc->context->ast_allocator, &value_exprs, type->static_array.count);

            auto elem_type = type->static_array.element_type;
            assert(elem_type->bit_size % 8 == 0);
            auto elem_size = elem_type->bit_size / 8;

            for (s64 i = 0; i < type->static_array.count; i++) {

                auto value_expr = interpreter_memory_to_ast_expression(bc, cursor, elem_type, scope, range);
                cursor += elem_size;
                dynamic_array_append(&value_exprs, value_expr);
            }

            result = ast_compound_expr_new(bc->context, range, value_exprs);
            break;
        }

        case Type_Kind::SLICE: assert(false); break;

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
#endif // NDEBUG
        type_resolve_expression(ctx->resolver, result, scope, infer_node);
    debug_assert(resolved_type);


    return result;
}

} }
