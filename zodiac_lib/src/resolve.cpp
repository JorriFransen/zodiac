#include "resolve.h"

#include "ast.h"
#include "atom.h"
#include "error.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "scope.h"
#include "type.h"
#include "util/asserts.h"
#include "zodiac_context.h"

namespace Zodiac
{

void resolver_create(Resolver *resolver, Zodiac_Context *ctx, Scope *global_scope)
{
    assert(resolver);
    assert(ctx);
    assert(global_scope);
    assert(global_scope->kind == Scope_Kind::GLOBAL);

    resolver->ctx = ctx;
    resolver->global_scope = global_scope;

    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_name_resolve);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_type_resolve);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_emit_bytecode);
}

#define add_builtin_type_symbol(type) { \
    auto sym = add_resolved_symbol(resolver->ctx, resolver->global_scope, Symbol_Kind::TYPE, (SYM_FLAG_BUILTIN), atom_##type, nullptr); \
    sym->builtin_type = &builtin_type_##type; \
}

void resolve_file(Resolver *resolver, AST_File *file)
{
    assert(file);

    add_builtin_type_symbol(u64);
    add_builtin_type_symbol(s64);
    add_builtin_type_symbol(u32);
    add_builtin_type_symbol(s32);
    add_builtin_type_symbol(u16);
    add_builtin_type_symbol(s16);
    add_builtin_type_symbol(u8);
    add_builtin_type_symbol(s8);

    add_builtin_type_symbol(r32);
    add_builtin_type_symbol(r64);

    add_builtin_type_symbol(void);
    add_builtin_type_symbol(bool);
    add_builtin_type_symbol(String);

    // Register all top level symbols first
    for (s64 i = 0; i < file->declarations.count; i++) {
        if (!add_unresolved_decl_symbol(resolver->ctx, resolver->global_scope, file->declarations[i], true)) {
            assert(resolver->ctx->fatal_resolve_error);
            assert(false);
            return;
        }
    }

    // Flatten all top level declarations, registering all nested symbols in the process
    for (s64 i = 0; i < file->declarations.count; i++) {
        resolver_add_declaration(resolver->ctx, resolver, file->declarations[i], resolver->global_scope);
    }

    bool done = false;

    while (!done) {

        Resolve_Results names_result = resolve_names(resolver);

        if (resolver->ctx->fatal_resolve_error) break;

        Resolve_Results types_result = resolve_types(resolver);

        if (resolver->ctx->fatal_resolve_error) break;

        done = ((names_result & RESOLVE_RESULT_DONE) == RESOLVE_RESULT_DONE) &&
               ((types_result & RESOLVE_RESULT_DONE) == RESOLVE_RESULT_DONE);

        bool progress = ((names_result & RESOLVE_RESULT_PROGRESS) == RESOLVE_RESULT_PROGRESS) &&
                        ((types_result & RESOLVE_RESULT_PROGRESS) == RESOLVE_RESULT_PROGRESS);

        if (done) assert(resolver->ctx->errors.count == 0);

        if (!progress && !done) {
            assert(resolver->ctx->errors.count);
            break;
        } else if (progress && resolve_error_count(resolver->ctx)) {
            resolver->ctx->errors.count = 0;
            temporary_allocator_reset(&resolver->ctx->error_allocator_state);
        }
    }


}

void resolver_add_declaration(Zodiac_Context *ctx, Resolver *resolver, AST_Declaration *decl, Scope *scope)
{
    assert(resolver);
    assert(decl);
    assert(scope);

    Flat_Root_Node node = {};
    node.root.kind = Flat_Node_Kind::DECL;
    node.root.decl = decl;
    node.current_index  = 0;

    dynamic_array_create(&dynamic_allocator, &node.nodes);

    if (decl->kind == AST_Declaration_Kind::FUNCTION) {
        Flat_Root_Node proto_node = {};
        proto_node.root.kind = Flat_Node_Kind::FUNCTION_PROTO;
        proto_node.root.decl = decl;
        proto_node.current_index = 0;

        dynamic_array_create(&dynamic_allocator, &proto_node.nodes);

        for (s64 i = 0; i < decl->function.params.count; i++) {
            auto field = decl->function.params[i];
            assert(field->kind == AST_Declaration_Kind::PARAMETER);

            flatten_type_spec(field->field.type_spec, scope, &proto_node.nodes);
        }

        if (decl->function.return_ts) {
            flatten_type_spec(decl->function.return_ts, scope, &proto_node.nodes);
        }

        Flat_Node flat_decl = to_flat_proto(decl, scope);
        dynamic_array_append(&proto_node.nodes, flat_decl);

        dynamic_array_append(&resolver->nodes_to_name_resolve, proto_node);
    }

    flatten_declaration(ctx, decl, resolver->global_scope, &node.nodes);

    dynamic_array_append(&resolver->nodes_to_name_resolve, node);
}

Resolve_Results resolve_names(Resolver *resolver)
{
    bool progress = false;
    bool done = true;

    for (u64 flat_index = 0; flat_index < resolver->nodes_to_name_resolve.count; flat_index++)
    {

        Flat_Root_Node *node = &resolver->nodes_to_name_resolve[flat_index];

        for (u64 i = node->current_index; i < node->nodes.count; i++)
        {

            node->current_index = i;
            bool result = name_resolve_node(resolver, &node->nodes[i]);

            if (!result)
            {
                done = false;
                break;
            }
            else
            {
                node->current_index += 1;
                progress = true;
            }
        }

        if (node->current_index >= node->nodes.count)
        {
            // Done name resolving, move to type resolving
            node->current_index = 0;

            auto node_copy = *node;

            dynamic_array_remove_ordered(&resolver->nodes_to_name_resolve, flat_index);
            flat_index -= 1;

            dynamic_array_append(&resolver->nodes_to_type_resolve, node_copy);
        }
    }

    // if (progress && resolve_error_count(resolver->ctx))
    // {
    //     assert(!done);

    //     // TODO: CLEANUP: Is it actually ok to clear all errors here?
    //     resolver->ctx->errors.count = 0;

    //     temporary_allocator_reset(&resolver->ctx->error_allocator_state);
    // }

    Resolve_Results result = RESOLVE_RESULT_NONE;
    if (done) result |= RESOLVE_RESULT_DONE;
    if (progress) result |= RESOLVE_RESULT_PROGRESS;

    return result;
}

Resolve_Results resolve_types(Resolver *resolver)
{
    bool progress = false;
    bool done = true;

    for (u64 flat_index = 0; flat_index < resolver->nodes_to_type_resolve.count; flat_index++)
    {

        Flat_Root_Node *node = &resolver->nodes_to_type_resolve[flat_index];

        for (u64 i = node->current_index; i < node->nodes.count; i++)
        {

            node->current_index = i;
            bool result = type_resolve_node(resolver->ctx, &node->nodes[i]);

            if (!result)
            {
                done = false;
                break;
            }
            else
            {
                node->current_index += 1;
                progress = true;
            }
        }

        if (node->current_index >= node->nodes.count)
        {
            // Done type resolving, move to bytecode emit
            node->current_index = 0;

            auto node_copy = *node;

            dynamic_array_remove_ordered(&resolver->nodes_to_type_resolve, flat_index);
            flat_index -= 1;

            dynamic_array_append(&resolver->nodes_to_emit_bytecode, node_copy);
        }
    }

    // if (progress && resolve_error_count(resolver->ctx))
    // {
    //     assert(!done);

    //     // TODO: CLEANUP: Is it actually ok to clear all errors here?
    //     resolver->ctx->errors.count = 0;

    //     temporary_allocator_reset(&resolver->ctx->error_allocator_state);
    // }

    Resolve_Results result = RESOLVE_RESULT_NONE;
    if (done) result |= RESOLVE_RESULT_DONE;
    if (progress) result |= RESOLVE_RESULT_PROGRESS;

    return result;
}

void flatten_declaration(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(decl && scope && dest);

    assert(decl->identifier.name.data);
    auto decl_sym = scope_get_symbol(scope, decl->identifier.name);
    if (decl_sym && decl_sym->decl != decl) {
        report_redecl(ctx, decl_sym->range, decl->identifier.name, decl->identifier.range);
        return;
    }

    if (scope->kind == Scope_Kind::GLOBAL && !decl_sym) {
        assert_msg(decl_sym, "Global symbol should have been registered already");
    } else if (!decl_sym) {
        // First time local symbol is encountered
        if (!add_unresolved_decl_symbol(ctx, scope, decl, false)) {
            return;
        }
        decl_sym = scope_get_symbol(scope, decl->identifier);
    }
    assert(decl_sym && decl_sym->decl == decl);

    Scope *aggregate_scope = nullptr;
    Scope *parameter_scope = nullptr;
    Scope *local_scope = nullptr;

    switch (decl_sym->kind) {

        default: break;

        case Symbol_Kind::FUNC: {
            assert(decl_sym->func.parameter_scope && decl_sym->func.local_scope);
            parameter_scope = decl_sym->func.parameter_scope;
            local_scope = decl_sym->func.local_scope;
            break;
        }

        case Symbol_Kind::TYPE: {
            assert(decl_sym->aggregate.scope);
            aggregate_scope = decl_sym->aggregate.scope;
            break;
        }

    }

    assert(decl_sym->state == Symbol_State::UNRESOLVED);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE:
        case AST_Declaration_Kind::PARAMETER:
        case AST_Declaration_Kind::FIELD: {
            auto ts = decl->variable.type_spec;
            auto val = decl->variable.value;

            if (ts) flatten_type_spec(ts, scope, dest);
            if (val) flatten_expression(val, scope, dest, ts);

            assert(decl->identifier.scope == nullptr);
            decl->identifier.scope = scope;
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            assert(parameter_scope);
            assert(local_scope);

            decl->function.parameter_scope = parameter_scope;
            decl->function.local_scope = local_scope;

            for (u64 i = 0; i < decl->function.params.count; i++) {

                auto field = decl->function.params[i];
                assert(field->kind == AST_Declaration_Kind::PARAMETER);

                flatten_type_spec(field->parameter.type_spec, scope, dest);
                Flat_Node param_node = to_flat_node(field, parameter_scope);
                dynamic_array_append(dest, param_node);
            }

            if (decl->function.return_ts) {
                flatten_type_spec(decl->function.return_ts, scope, dest);
            }

            for (u64 i = 0; i < decl->function.body.count; i++) {
                flatten_statement(ctx, decl->function.body[i], local_scope, dest);
            }

            break;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            assert(aggregate_scope);
            for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
                auto field = decl->aggregate.fields[i];
                assert(field->kind == AST_Declaration_Kind::FIELD);

                flatten_type_spec(field->field.type_spec, scope, dest);
                Flat_Node member_node = to_flat_node(field, aggregate_scope);
                dynamic_array_append(dest, member_node);
            }

            break;
        }
    }

    Flat_Node flat_decl = to_flat_node(decl, scope);
    dynamic_array_append(dest, flat_decl);
}

void flatten_statement(Zodiac_Context *ctx, AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(stmt && scope && dest);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {

            Scope *block_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
            stmt->block.scope = block_scope;

            for (u64 i = 0; i < stmt->block.statements.count; i++) {
                flatten_statement(ctx, stmt->block.statements[i], block_scope, dest);
            }
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            flatten_declaration(ctx, stmt->decl.decl, scope, dest);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            flatten_expression(stmt->assign.dest, scope, dest, nullptr);
            flatten_expression(stmt->assign.value, scope, dest, nullptr);
            break;
        }

        case AST_Statement_Kind::CALL: {
            flatten_expression(stmt->call.call, scope, dest, nullptr);
            break;
        }

        case AST_Statement_Kind::IF: {
            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = &stmt->if_stmt.blocks[i];

                flatten_expression(if_block->cond, scope, dest, nullptr);

                auto then_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
                assert(if_block->then_scope == nullptr);
                if_block->then_scope = then_scope;
                flatten_statement(ctx, if_block->then, then_scope, dest);
            }

            if (stmt->if_stmt.else_stmt) {
                auto else_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
                flatten_statement(ctx, stmt->if_stmt.else_stmt, else_scope, dest);
                assert(stmt->if_stmt.else_scope == nullptr);
                stmt->if_stmt.else_scope = else_scope;
            }
            break;
        }

        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                AST_Declaration *func_decl = enclosing_function(scope);
                assert(func_decl);

                AST_Type_Spec *infer_from = nullptr;

                if (func_decl->function.return_ts) {
                    infer_from = func_decl->function.return_ts;
                }

                stmt->return_stmt.scope = scope;

                flatten_expression(stmt->return_stmt.value, scope, dest, infer_from);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            flatten_expression(stmt->print_expr, scope, dest, nullptr);
            break;
        }
    }

    Flat_Node flat_stmt = to_flat_node(stmt, scope);
    dynamic_array_append(dest, flat_stmt);
}

void flatten_expression(AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest, AST_Type_Spec *infer_type_from)
{
    assert(expr && scope && dest);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL: {
            expr->infer_type_from = infer_type_from;
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL: {
            // Leaf expression
            break;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            assert(expr->identifier.scope == nullptr);
            expr->identifier.scope = scope;
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            assert(!infer_type_from);
            flatten_expression(expr->member.base, scope, dest, nullptr);
            break;
        }

        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {
            flatten_expression(expr->call.base, scope, dest, nullptr);

            for (u64 i = 0 ; i < expr->call.args.count; i++) {
                flatten_expression(expr->call.args[i], scope, dest, nullptr);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            // assert(infer_type_from);
            flatten_expression(expr->binary.lhs, scope, dest, infer_type_from);
            flatten_expression(expr->binary.rhs, scope, dest, infer_type_from);
            expr->infer_type_from = infer_type_from;
            break;
        }
    }

    Flat_Node flat_expr = to_flat_node(expr, scope);
    dynamic_array_append(dest, flat_expr);
}

void flatten_type_spec(AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(ts && scope && dest);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            // Leaf
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            flatten_type_spec(ts->base, scope, dest);
            break;
        }
    }

    Flat_Node flat_ts = to_flat_node(ts, scope);
    dynamic_array_append(dest, flat_ts);
}

Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope)
{
    assert(decl && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::DECL;
    result.scope = scope;
    result.decl = decl;
    return result;
}

Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope)
{
    assert(stmt && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::STMT;
    result.scope = scope;
    result.stmt = stmt;
    return result;
}

Flat_Node to_flat_node(AST_Expression *expr, Scope *scope)
{
    assert(expr && scope);

    Flat_Node result = {};
    result.scope = scope;
    result.kind = Flat_Node_Kind::EXPR;
    result.expr = expr;
    return result;
}

Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope)
{
    assert(ts && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TS;
    result.scope = scope;
    result.ts = ts;
    return result;
}

Flat_Node to_flat_proto(AST_Declaration *decl, Scope *scope)
{
    assert(decl && decl->kind == AST_Declaration_Kind::FUNCTION);
    assert(scope && scope->kind == Scope_Kind::GLOBAL);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::FUNCTION_PROTO;
    result.decl = decl;
    result.scope = scope;

    return result;
}

bool name_resolve_node(Resolver *resolver, Flat_Node *node)
{
    assert(resolver);
    assert(node);

    switch (node->kind) {
        case Flat_Node_Kind::DECL: {
            return name_resolve_decl(resolver, node->decl, node->scope);
        }

        case Flat_Node_Kind::STMT: {
            return name_resolve_stmt(node->stmt, node->scope);
        }

        case Flat_Node_Kind::EXPR: {
            return name_resolve_expr(resolver->ctx, node->expr, node->scope);
        }

        case Flat_Node_Kind::TS: {
            return name_resolve_ts(resolver->ctx, node->ts, node->scope);
        }

        case Flat_Node_Kind::PARAM_DECL: {
            Symbol *param_sym = scope_get_symbol(node->scope, node->decl->identifier);
            assert(param_sym);
            assert(param_sym->decl->kind == AST_Declaration_Kind::FUNCTION);

            assert(param_sym->state == Symbol_State::UNRESOLVED);
            param_sym->state = Symbol_State::RESOLVED;
            return true;
        }

        case Flat_Node_Kind::FIELD_DECL: {
            assert(node->scope->kind == Scope_Kind::AGGREGATE);
            Symbol *field_sym = scope_get_symbol(node->scope, node->decl->identifier);
            assert(field_sym);
            assert(field_sym->decl->kind == AST_Declaration_Kind::STRUCT ||
                   field_sym->decl->kind == AST_Declaration_Kind::UNION);

            assert(field_sym->state == Symbol_State::UNRESOLVED);
            field_sym->state = Symbol_State::RESOLVED;
            return true;
        }

        case Flat_Node_Kind::FUNCTION_PROTO: {
            // If we get here, we have name resolved all dependencies
            Symbol *sym = scope_get_symbol(node->scope, node->decl->identifier);
            assert(sym && sym->kind == Symbol_Kind::FUNC);
            assert(sym->state == Symbol_State::UNRESOLVED);
            sym->state = Symbol_State::RESOLVED;
            return true;
        }
    }

    assert(false);
    return false;
}

bool name_resolve_decl(Resolver *resolver, AST_Declaration *decl, Scope *scope)
{
    assert(resolver);
    assert(decl && scope);

    assert(decl->identifier.name.data);
    auto decl_sym = scope_get_symbol(scope, decl->identifier.name);

    bool global = decl->flags & AST_DECL_FLAG_GLOBAL;

    assert(global == (scope->kind == Scope_Kind::GLOBAL));

    if (!decl_sym) {
        assert_msg(scope->kind != Scope_Kind::GLOBAL, "Global declaration should have been defined");
        assert_msg(false, "TODO: local declaration symbol");
    }

    if (decl_sym->decl != decl) {
        assert_msg(false, "Redeclaration?");
        return false;
    }

    assert(decl_sym);

    switch (decl_sym->state) {

        case Symbol_State::UNRESOLVED: {
            decl_sym->state = Symbol_State::RESOLVING;
            break;
        }

        case Symbol_State::RESOLVING: assert_msg(false, "Circular dependency");

        case Symbol_State::RESOLVED:
        case Symbol_State::TYPED:
        {
            return true;
        }
    }

    bool result = true;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {
            if (global) {

                auto init_expr = decl->variable.value;
                if (init_expr && !EXPR_IS_CONST(init_expr)) {
                    resolve_error(resolver->ctx, init_expr, "Global initializer must be a constant");
                    assert(decl_sym->state == Symbol_State::RESOLVING);
                    decl_sym->state = Symbol_State::UNRESOLVED;
                    return false;
                }

            } else {
                AST_Declaration *func_decl = enclosing_function(scope);
                dynamic_array_append(&func_decl->function.variables, decl);
            }
            break;
        };

        case AST_Declaration_Kind::CONSTANT_VARIABLE:
        case AST_Declaration_Kind::PARAMETER:
        case AST_Declaration_Kind::FIELD:
        case AST_Declaration_Kind::FUNCTION:
        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            // Leaf
            break;
        }

    }


    decl_sym = scope_get_symbol(scope, decl->identifier.name);
    assert(decl_sym);
    if (result) {
        decl_sym->state = Symbol_State::RESOLVED;
    } else {
        decl_sym->state = Symbol_State::UNRESOLVED;
    }

    return result;
}

bool name_resolve_stmt(AST_Statement *stmt, Scope *scope)
{
    assert(stmt && scope);

    bool result = true;

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);


        case AST_Statement_Kind::BLOCK: {
            // Leaf, statements are added during flattening, should have been resolved already
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            // Leaf, Symbol is added during flattening, value should have been resolved already
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            // Leaf, decl sym and value should have been resolved already
            break;
        }

        case AST_Statement_Kind::CALL: {
            // Leaf, base expr and args should have been resolved already
            break;
        }

        case AST_Statement_Kind::IF: break; // all resolved
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            // Leaf, optional value should have been resolved already
            break;
        }

        case AST_Statement_Kind::PRINT: {
            // Leaf, value should have been resolved already
            break;
        }
    }

    return result;
}

bool name_resolve_expr(Zodiac_Context *ctx, AST_Expression *expr, Scope *scope)
{
    assert(expr && scope);

    bool result = true;

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL: {
            assert((expr->flags & AST_EXPR_FLAG_CONST) == AST_EXPR_FLAG_CONST);
            // Leaf
            break;
        }

        case AST_Expression_Kind::BINARY: {
            if (EXPR_IS_CONST(expr->binary.lhs) && EXPR_IS_CONST(expr->binary.rhs)) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }
            break;
        }

        case AST_Expression_Kind::CALL: {
            AST_Expression *base = expr->call.base;

            // TODO: Support arbitrary expressions here
            assert(base->kind == AST_Expression_Kind::IDENTIFIER);

            Symbol *sym = scope_get_symbol(scope, base->identifier);
            assert(sym);
            if (sym->kind != Symbol_Kind::FUNC) {
                resolve_error(ctx, base, "Not a function '%s'", sym->name.data);
                result = false;
                break;
            }

            assert(sym->kind == Symbol_Kind::FUNC);
            AST_Declaration *decl = sym->decl;
            assert(decl);
            break;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            Symbol *sym = scope_get_symbol(scope, expr->identifier.name);

            if (!sym) {
                resolve_error(ctx, expr, "Undefined symbol: '%s'", expr->identifier.name.data);
                result = false;
                break;
            }

            if (sym->state == Symbol_State::RESOLVING) {

                fatal_resolve_error(ctx, expr, "Circular dependency detected");
                result = false;
                break;

            } else if (sym->state == Symbol_State::UNRESOLVED) {

                resolve_error(ctx, expr, "Unresolved symbol: '%s'", expr->identifier.name.data);
                result = false;
                break;

            } else {
                assert(sym->state >= Symbol_State::RESOLVED);

                if (sym->kind == Symbol_Kind::CONST) {
                    expr->flags |= AST_EXPR_FLAG_CONST;
                }
            }
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            AST_Expression *base = expr->member.base;
            AST_Declaration *base_decl = nullptr;
            AST_Declaration *base_type_decl = nullptr;

            if (base->kind == AST_Expression_Kind::IDENTIFIER) {

                Symbol *base_sym = scope_get_symbol(scope, base->identifier);
                assert(base_sym->state == Symbol_State::RESOLVED);
                assert(base_sym->decl);
                base_decl = base_sym->decl;
                assert(base_decl->kind == AST_Declaration_Kind::VARIABLE);

            } else {
                assert(base->kind == AST_Expression_Kind::MEMBER);
                base_type_decl = base->member.type_decl;
            }

            Scope *base_scope = nullptr;

            if (base_decl->variable.type_spec) {

                auto ts = base_decl->variable.type_spec;
                assert(ts->kind == AST_Type_Spec_Kind::NAME);

                auto base_type_decl_sym = scope_get_symbol(scope, ts->identifier);
                assert(base_type_decl_sym);
                assert(base_type_decl_sym->kind == Symbol_Kind::TYPE);
                assert(base_type_decl_sym->decl);
                assert(base_type_decl_sym->aggregate.scope);

                base_type_decl = base_type_decl_sym->decl;
                base_scope = base_type_decl_sym->aggregate.scope;

            } else {
                assert(false);
            }

            assert(base_type_decl);
            assert(base_type_decl->kind == AST_Declaration_Kind::STRUCT);

            Symbol *mem_sym = scope_get_symbol(base_scope, expr->member.member_name);
            if (!mem_sym) {
                auto aggregate_name = base_type_decl->identifier.name.data;
                resolve_error(ctx, expr, "'%s' is not a member of '%s'", expr->member.member_name.data, aggregate_name);
                resolve_error(ctx, base_type_decl, "'%s' defined here", aggregate_name);
                result = false;
                break;
            }

            assert(mem_sym->decl);
            assert(expr->member.type_decl == nullptr);
            expr->member.type_decl = mem_sym->decl;


            assert(mem_sym && mem_sym->kind == Symbol_Kind::MEMBER);
            result = mem_sym->state == Symbol_State::RESOLVED;
            break;
        }

        case AST_Expression_Kind::INDEX: assert(false);
        case AST_Expression_Kind::UNARY: assert(false);
    }

    return result;
}

bool name_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope)
{
    assert(ts && scope);

    bool result = true;

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            Symbol *sym = scope_get_symbol(scope, ts->identifier.name);

            if (!sym) {
                resolve_error(ctx, ts, "Undefined symbol: '%s'", ts->identifier.name.data);
                result = false;
                break;
            }

            if (sym->kind != Symbol_Kind::TYPE) {
                resolve_error(ctx, ts, "Not a type: '%s'", ts->identifier.name.data);
                result = false;
                break;
            }

            if (sym->state == Symbol_State::RESOLVING) {

                fatal_resolve_error(ctx, ts, "Circular dependency detected");
                result = false;
                break;

            } else if (sym->state == Symbol_State::UNRESOLVED) {

                resolve_error(ctx, ts, "Unresolved symbol: '%s'", ts->identifier.name.data);
                result = false;
                break;

            } else {
                assert(sym->state == Symbol_State::RESOLVED);
            }

            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            // Leaf, the base should have been resolved before..
            break;
        }

    }

    return result;
}

bool type_resolve_node(Zodiac_Context *ctx, Flat_Node *node)
{
    assert(node);

    switch (node->kind) {

        case Flat_Node_Kind::DECL: {
            return type_resolve_declaration(ctx, node->decl, node->scope);
            break;
        }

        case Flat_Node_Kind::STMT: {
            return type_resolve_statement(node->stmt, node->scope);
            break;
        }

        case Flat_Node_Kind::EXPR: {
            return type_resolve_expression(ctx, node->expr, node->scope);
            break;
        }

        case Flat_Node_Kind::TS: {
            return type_resolve_ts(node->ts, node->scope);
        }

        case Flat_Node_Kind::PARAM_DECL: {
            assert(node->decl->parameter.type_spec);
            assert(node->decl->parameter.type_spec->resolved_type);

            assert(node->decl->parameter.resolved_type == nullptr);
            node->decl->parameter.resolved_type = node->decl->parameter.type_spec->resolved_type;

            auto sym = scope_get_symbol(node->scope, node->decl->identifier);
            assert(sym);
            assert(sym->kind == Symbol_Kind::PARAM);
            assert(sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case Flat_Node_Kind::FIELD_DECL: {
            assert(node->decl->field.type_spec);
            assert(node->decl->field.type_spec->resolved_type);

            assert(node->decl->field.resolved_type == nullptr);
            node->decl->field.resolved_type = node->decl->field.type_spec->resolved_type;

            auto sym = scope_get_symbol(node->scope, node->decl->identifier);
            assert(sym);
            assert(sym->kind == Symbol_Kind::MEMBER);
            assert(sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case Flat_Node_Kind::FUNCTION_PROTO: {

            AST_Declaration *func_decl = node->decl;
            assert(func_decl->kind == AST_Declaration_Kind::FUNCTION);

            if (func_decl->function.type) {
                if (func_decl->function.return_ts) {
                    assert(func_decl->function.return_ts->resolved_type);
                    assert(func_decl->function.type->function.return_type == func_decl->function.return_ts->resolved_type) ;
                }
            } else {

                if (!func_decl->function.return_ts &&
                    !func_decl->function.inferred_return_type)
                {
                    resolve_error(ctx, node->decl, "Could not infer return type");
                    return false;
                }

                auto param_types = temp_array_create<Type *>(&ctx->temp_allocator, func_decl->function.params.count);

                for (u64 i = 0; i < func_decl->function.params.count; i++) {

                    // Only use the typespec here, the actual fields are not resolved as part of the prototype
                    auto param_ts = func_decl->function.params[i]->parameter.type_spec;
                    assert(param_ts && param_ts->resolved_type);

                    dynamic_array_append(&param_types.array, param_ts->resolved_type);
                }

                Type *return_type = nullptr;
                if (func_decl->function.return_ts) {
                    assert(func_decl->function.return_ts->resolved_type)
                    return_type = func_decl->function.return_ts->resolved_type;
                }

                if (func_decl->function.inferred_return_type) {
                    return_type = func_decl->function.inferred_return_type;
                }

                if (!return_type) {
                    fatal_resolve_error(ctx, func_decl, "Could not infer return type");
                    return false;
                }

                func_decl->function.type = get_function_type(return_type, param_types.array, &ctx->ast_allocator);

                temp_array_destroy(&param_types);
            }

            auto sym = scope_get_symbol(node->scope, node->decl->identifier.name);
            assert(sym && sym->kind == Symbol_Kind::FUNC);
            assert(sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

    }

    assert(false);
    return false;
}

bool type_resolve_declaration(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope)
{
    assert(decl);
    assert(scope);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {
            assert(decl->variable.resolved_type == nullptr);

            if (decl->variable.type_spec && decl->variable.value) {

                // Type and value
                assert(valid_static_type_conversion(decl->variable.value->resolved_type,
                                                    decl->variable.type_spec->resolved_type));
                decl->variable.resolved_type = decl->variable.type_spec->resolved_type;

            } else if ((!decl->variable.type_spec) && decl->variable.value) {

                // Value only
                assert(decl->variable.value->resolved_type);
                if (decl->variable.value->resolved_type == &builtin_type_unsized_integer) {

                    // TODO: Check if this fits
                    decl->variable.resolved_type = &builtin_type_s64;
                } else {

                    assert(decl->variable.value->resolved_type);
                    decl->variable.resolved_type = decl->variable.value->resolved_type;
                }

            } else {

                // Type spec only
                assert(decl->variable.type_spec);
                decl->variable.resolved_type = decl->variable.type_spec->resolved_type;
            }

            assert(decl->variable.resolved_type);

            if (decl->variable.value && scope->kind == Scope_Kind::GLOBAL) {
                assert(EXPR_IS_CONST(decl->variable.value));
            }

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            assert(decl->variable.resolved_type == nullptr);
            assert(decl->variable.value);
            assert(EXPR_IS_CONST(decl->variable.value));

            if (decl->variable.type_spec) {
                assert(valid_static_type_conversion(decl->variable.value->resolved_type,
                                                    decl->variable.type_spec->resolved_type));
                decl->variable.resolved_type = decl->variable.type_spec->resolved_type;
            } else {
                decl->variable.resolved_type = decl->variable.value->resolved_type;
            }

            assert(decl->variable.resolved_type);

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case AST_Declaration_Kind::PARAMETER: {
            assert(decl->parameter.resolved_type == nullptr);
            assert(decl->parameter.value == nullptr);
            assert(decl->parameter.type_spec);
            assert(decl->parameter.type_spec->resolved_type);

            decl->parameter.resolved_type = decl->parameter.type_spec->resolved_type;

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            assert(sym->kind == Symbol_Kind::PARAM);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case AST_Declaration_Kind::FIELD: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {

            // Only implicitly infer a void return type from the body when there is no return type_spec
            if (!decl->function.type && !decl->function.inferred_return_type) {
                if (!decl->function.return_ts) {
                    auto param_types = temp_array_create<Type *>(&ctx->temp_allocator);

                    for (s64 i = 0; i < decl->function.params.count; i++) {
                        auto param = decl->function.params[i];
                        assert(param->kind == AST_Declaration_Kind::PARAMETER);
                        assert(param->parameter.resolved_type);
                        dynamic_array_append(&param_types.array, param->parameter.resolved_type);
                    }
                    decl->function.type = get_function_type(&builtin_type_void, param_types.array, &ctx->ast_allocator);
                    decl->function.inferred_return_type = decl->function.type->function.return_type;

                    temp_array_destroy(&param_types);
                } else {
                    assert(false);
                }
            }

            if (decl->function.type) assert(decl->function.type->kind == Type_Kind::FUNCTION);
            return decl->function.type != nullptr;
        }

        case AST_Declaration_Kind::STRUCT: {
            assert(!decl->aggregate.resolved_type);

            auto temp_member_types = temp_array_create<Type *>(temp_allocator_allocator(), decl->aggregate.fields.count);

            for (s64 i = 0; i < decl->aggregate.fields.count; i++) {
                auto field = decl->aggregate.fields[i];
                assert(field->kind == AST_Declaration_Kind::FIELD);
                assert(field->field.resolved_type);

                dynamic_array_append(&temp_member_types.array, field->field.resolved_type);
            }

            auto member_types = temp_array_finalize(&ctx->ast_allocator, &temp_member_types);

            decl->aggregate.resolved_type = get_struct_type(ctx, member_types, decl->identifier.name.data, &ctx->ast_allocator);
            return true;
        }

        case AST_Declaration_Kind::UNION: assert(false);
    }

    assert(false);
    return false;
}

bool type_resolve_statement(AST_Statement *stmt, Scope *scope)
{
    assert(stmt);
    assert(scope);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {
            // leaf
            return true;
        }

        case AST_Statement_Kind::DECLARATION: {
            // This declaration should have been emitted before the statement, and therefore resolved if we got to this point.
            return true;
        }

        case AST_Statement_Kind::ASSIGN: assert(false);

        case AST_Statement_Kind::CALL: {
            // leaf
            return true;
        }

        case AST_Statement_Kind::IF: {

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto &if_block = stmt->if_stmt.blocks[i];
                assert(if_block.cond->resolved_type);
                assert(if_block.cond->resolved_type->kind == Type_Kind::BOOLEAN);
            }

            return true;
        }

        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            AST_Declaration *fn_decl = enclosing_function(scope);
            assert(fn_decl && fn_decl->kind == AST_Declaration_Kind::FUNCTION);

            if (fn_decl->function.return_ts && !fn_decl->function.type) {
                assert(false); // Wait for this to be resolved first!
            }

            Type *fn_type = fn_decl->function.type;
            if (fn_type) assert(fn_type->kind == Type_Kind::FUNCTION);

            if (stmt->return_stmt.value) {

                auto actual_type = stmt->return_stmt.value->resolved_type;

                if (fn_type) {
                    auto expected_type = fn_type->function.return_type;

                    assert(actual_type && expected_type);

                    if (actual_type == expected_type) return true;

                    bool valid_conversion = valid_static_type_conversion(actual_type, expected_type);
                    if (!valid_conversion) {
                        assert(false); // report error
                        return false;
                    }
                } else {

                    if (actual_type == &builtin_type_unsized_integer) {
                        actual_type = &builtin_type_s64;
                    }

                    if (fn_decl->function.inferred_return_type) {
                        assert(fn_decl->function.inferred_return_type == actual_type);
                    } else {
                        fn_decl->function.inferred_return_type = actual_type;
                    }
                }

            } else {
                fn_decl->function.inferred_return_type = &builtin_type_void;
            }

            return true;
        }

        case AST_Statement_Kind::PRINT: {
            auto type = stmt->print_expr->resolved_type;
            assert(type);

            if (type->kind == Type_Kind::UNSIZED_INTEGER) {
                type = &builtin_type_s64;
                stmt->print_expr->resolved_type = &builtin_type_s64;
            }

            assert(type->kind == Type_Kind::INTEGER ||
                   type->kind == Type_Kind::BOOLEAN ||
                   type == &builtin_type_String);

            return true;
        }
    }

    assert(false);
    return false;
}

bool type_resolve_expression(Zodiac_Context *ctx, AST_Expression *expr, Scope *scope)
{
    assert(expr);
    assert(expr->resolved_type == nullptr);
    assert(scope);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL: {
                AST_Type_Spec *infer_from = expr->infer_type_from;
                if (infer_from) {
                    assert(infer_from);
                    assert(infer_from->resolved_type);
                    assert(infer_from->resolved_type->kind == Type_Kind::INTEGER);
                    // TODO: Make sure the literal fits in this type

                    expr->resolved_type = infer_from->resolved_type;
                } else {
                    expr->resolved_type = &builtin_type_unsized_integer;
                }
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            assert(!expr->infer_type_from);
            expr->resolved_type = &builtin_type_String;
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::BOOL_LITERAL: {
            assert(!expr->infer_type_from);
            expr->resolved_type = &builtin_type_bool;
            break;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            auto sym = scope_get_symbol(scope, expr->identifier.name);
            assert(sym);
            if (sym->state != Symbol_State::TYPED) {
                resolve_error(ctx, expr, "Waiting for declaration to be resolved");
                return false;
            }

            expr->resolved_type = sym_decl_type(sym);

            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {

            auto ident_expr = expr->call.base;

            auto func_sym = scope_get_symbol(ident_expr->identifier.scope, ident_expr->identifier.name);
            assert(func_sym && func_sym->kind == Symbol_Kind::FUNC);
            assert(func_sym->state == Symbol_State::TYPED);

            auto func_decl = func_sym->decl;
            assert(func_decl && func_decl->kind == AST_Declaration_Kind::FUNCTION);
            assert(func_decl->function.type && func_decl->function.type->kind == Type_Kind::FUNCTION);

            if (expr->call.args.count != func_decl->function.params.count) {
                resolve_error(ctx, expr, "Expected %i arguments, got %i", func_decl->function.params.count, expr->call.args.count);
                return false;
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {
                AST_Expression *arg_expr = expr->call.args[i];
                Type *arg_type = arg_expr->resolved_type;

                Type *param_type = func_decl->function.params[i]->parameter.resolved_type;

                bool match = true;

                if (arg_type != param_type) {
                    if (valid_static_type_conversion(arg_type, param_type)) {
                        arg_expr->resolved_type = param_type;
                    } else {
                        match = false;
                    }
                }

                if (!match) {
                    resolve_error(ctx, arg_expr, "Mismatching type for argument %i", i + 1);
                    resolve_error(ctx, arg_expr, "    Expected: %s", temp_type_string(param_type));
                    resolve_error(ctx, arg_expr, "    Got: %s", temp_type_string(arg_type));
                    return false;
                }
            }

            auto return_type = func_decl->function.type->function.return_type;
            assert(return_type);

            expr->resolved_type = return_type;
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            auto lhs = expr->binary.lhs;
            auto rhs = expr->binary.rhs;
            auto op = expr->binary.op;

            assert(lhs->resolved_type);
            assert(rhs->resolved_type);

            if (is_binary_arithmetic_op(op)) {
                assert(lhs->resolved_type->flags & TYPE_FLAG_INT);
                assert(rhs->resolved_type->flags & TYPE_FLAG_INT);

                if (lhs->resolved_type->kind == Type_Kind::INTEGER &&
                    rhs->resolved_type->kind == Type_Kind::UNSIZED_INTEGER) {

                    if (valid_static_type_conversion(rhs->resolved_type, lhs->resolved_type)) {
                        expr->resolved_type = lhs->resolved_type;
                        rhs->resolved_type = lhs->resolved_type;
                    } else {
                        assert(false); // Report error
                    }

                } else if (lhs->resolved_type->kind == Type_Kind::UNSIZED_INTEGER &&
                           rhs->resolved_type->kind == Type_Kind::INTEGER) {

                    if (valid_static_type_conversion(lhs->resolved_type, rhs->resolved_type)) {
                        expr->resolved_type = rhs->resolved_type;
                        lhs->resolved_type = rhs->resolved_type;
                    } else {
                        assert(false); // Report error
                    }

                } else if (lhs->resolved_type->kind == Type_Kind::INTEGER &&
                            rhs->resolved_type->kind == Type_Kind::INTEGER) {
                    assert(lhs->resolved_type == rhs->resolved_type);
                    expr->resolved_type = lhs->resolved_type;
                } else if (expr->infer_type_from){
                    assert(expr->infer_type_from);
                    assert(expr->infer_type_from->resolved_type);
                    assert(expr->infer_type_from->resolved_type->kind == Type_Kind::INTEGER);
                    expr->resolved_type = expr->infer_type_from->resolved_type;
                } else {
                    assert(lhs->resolved_type == &builtin_type_unsized_integer);
                    assert(rhs->resolved_type == &builtin_type_unsized_integer);
                    expr->resolved_type = &builtin_type_s64;
                }

            } else if (is_binary_cmp_op(op)) {
                if (lhs->resolved_type->kind == Type_Kind::INTEGER &&
                    rhs->resolved_type->kind == Type_Kind::UNSIZED_INTEGER) {

                    assert(valid_static_type_conversion(rhs->resolved_type, lhs->resolved_type));
                    rhs->resolved_type = lhs->resolved_type;
                }
                assert(lhs->resolved_type == rhs->resolved_type);
                if (lhs->resolved_type == &builtin_type_unsized_integer) {
                    lhs->resolved_type = &builtin_type_s64;
                    rhs->resolved_type = &builtin_type_s64;
                }
                expr->resolved_type = &builtin_type_bool;

            } else {
                assert(false);
            }

            break;
        }

    }

    assert(expr->resolved_type);
    return true;
}

bool type_resolve_ts(AST_Type_Spec *ts, Scope *scope)
{
    assert(ts && scope);

    if (ts->resolved_type) return true;

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            auto sym = scope_get_symbol(scope, ts->identifier.name);
            if (sym->decl) {

                assert(sym->kind == Symbol_Kind::TYPE);
                assert(sym->decl);

                auto type = sym_decl_type(sym);
                assert((type->flags & TYPE_FLAG_AGGREGATE) == TYPE_FLAG_AGGREGATE);
                ts->resolved_type = type;
                return true;

            } else {
                assert((sym->flags & SYM_FLAG_BUILTIN) == SYM_FLAG_BUILTIN)
                assert(sym->builtin_type);
                ts->resolved_type = sym->builtin_type;
                return true;
            }

            assert(false); // Should have returned
        }

        case AST_Type_Spec_Kind::POINTER: assert(false);
    }

    assert(false);
    return false;
}

}
