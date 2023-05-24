#include "resolve.h"

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
}

void resolver_add_declaration(Zodiac_Context *ctx, Resolver *resolver, AST_Declaration *decl)
{
    assert(resolver);
    assert(decl);

    Flat_Root_Node node = {};
    node.root.kind = Flat_Node_Kind::DECL;
    node.root.decl = decl;
    node.current_index  = 0;

    dynamic_array_create(&dynamic_allocator, &node.nodes);

    if (!add_unresolved_decl_symbol(ctx, resolver->global_scope, decl, true)) {
        assert(ctx->fatal_resolve_error);
        assert(false);
        return;
    }

    flatten_declaration(ctx, decl, resolver->global_scope, &node.nodes);

    dynamic_array_append(&resolver->nodes_to_name_resolve, node);
}

bool resolve_names(Resolver *resolver)
{
    bool progress = true;
    bool done = false;

    while (progress && !done && !resolver->ctx->fatal_resolve_error) {
        progress = false;
        done = true;

        for (u64 flat_index = 0; flat_index < resolver->nodes_to_name_resolve.count; flat_index++) {

            Flat_Root_Node *node = &resolver->nodes_to_name_resolve[flat_index];

            for (u64 i = node->current_index; i < node->nodes.count; i++) {

                node->current_index = i;
                bool result = name_resolve_node(resolver->ctx, &node->nodes[i]);

                if (!result) {
                    done = false;
                    break;
                } else {
                    node->current_index += 1;
                    progress = true;
                }
            }

            if (node->current_index >= node->nodes.count) {
                // Done name resolving, move to type resolving
                node->current_index = 0;

                auto node_copy = *node;

                // Because we remove unordered, reuse the current index
                dynamic_array_remove_unordered(&resolver->nodes_to_name_resolve, flat_index);
                flat_index--;


                dynamic_array_append(&resolver->nodes_to_type_resolve, node_copy);
            }

        }

        if (progress && resolve_error_count(resolver->ctx)) {
            assert(!done);

            // TODO: CLEANUP: Is it actually ok to clear all errors here?
            resolver->ctx->errors.count = 0;

            temporary_allocator_reset(&resolver->ctx->error_allocator_state);
        }
    }

    return done;
}

void resolve_types(Resolver *resolver)
{
    bool progress = true;
    bool done = false;

    while (progress && !done && !resolver->ctx->fatal_resolve_error) {
        progress = false;
        done = true;

        for (u64 flat_index = 0; flat_index < resolver->nodes_to_type_resolve.count; flat_index++) {

            Flat_Root_Node *node = &resolver->nodes_to_type_resolve[flat_index];

            for (u64 i = node->current_index; i < node->nodes.count; i++) {

                node->current_index = i;
                bool result = type_resolve_node(resolver->ctx, &node->nodes[i]);

                if (!result) {
                    done = false;
                    break;
                } else {
                    node->current_index += 1;
                    progress = true;
                }
            }

            if (node->current_index >= node->nodes.count) {
                // Done name resolving, move to type resolving
                node->current_index = 0;

                // Because we remove unordered, reuse the current index
                dynamic_array_remove_unordered(&resolver->nodes_to_type_resolve, flat_index);
                flat_index--;

                // dynamic_array_append(&resolver->nodes_to_type_resolve, *node);
            }
        }

        if (progress && resolve_error_count(resolver->ctx)) {
            assert(!done);

            // TODO: CLEANUP: Is it actually ok to clear all errors here?
            resolver->ctx->errors.count = 0;

            temporary_allocator_reset(&resolver->ctx->error_allocator_state);
        }
    }
}

void flatten_declaration(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(decl && scope && dest);

    assert(decl->identifier.name.data);
    auto decl_sym = scope_get_symbol(scope, decl->identifier.name);
    if (decl_sym && decl_sym->decl != decl) {
        report_redecl(ctx, decl_sym->range, decl->identifier.name, decl->identifier.pos);
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
        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->variable.type_spec;
            auto val = decl->variable.value;

            if (ts) flatten_type_spec(ts, scope, dest);
            if (val) flatten_expression(val, scope, dest);

            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            assert(parameter_scope);
            assert(local_scope);

            for (u64 i = 0; i < decl->function.params.count; i++) {

                auto field = decl->function.params[i];

                flatten_type_spec(field.type_spec, scope, dest);
                Flat_Node param_node = to_flat_node(field, parameter_scope);
                dynamic_array_append(dest, param_node);
            }

            if (decl->function.return_ts) {
                flatten_type_spec(decl->function.return_ts, scope, dest);
            }

            Flat_Node proto_node = to_flat_proto(decl);
            dynamic_array_append(dest, proto_node);

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

                flatten_type_spec(field.type_spec, scope, dest);
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
            flatten_expression(stmt->assign.dest, scope, dest);
            flatten_expression(stmt->assign.value, scope, dest);
            break;
        }

        case AST_Statement_Kind::CALL: {
            flatten_expression(stmt->call.call, scope, dest);
            break;
        }

        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                flatten_expression(stmt->return_stmt.value, scope, dest);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            flatten_expression(stmt->print_expr, scope, dest);
            break;
        }
    }

    Flat_Node flat_stmt = to_flat_node(stmt, scope);
    dynamic_array_append(dest, flat_stmt);
}

void flatten_expression(AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(expr && scope && dest);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::IDENTIFIER: {
            // Leaf expression
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {
            flatten_expression(expr->call.base, scope, dest);

            for (u64 i = 0 ; i < expr->call.args.count; i++) {
                flatten_expression(expr->call.args[i], scope, dest);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            flatten_expression(expr->binary.lhs, scope, dest);
            flatten_expression(expr->binary.rhs, scope, dest);
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

Flat_Node to_flat_node(const AST_Field_Declaration param, Scope *scope)
{
    assert(scope);
    assert(scope->kind == Scope_Kind::FUNCTION_PARAMETER ||
           scope->kind == Scope_Kind::AGGREGATE);

    Flat_Node result = {};
    if (scope->kind == Scope_Kind::FUNCTION_PARAMETER) {
        result.kind = Flat_Node_Kind::PARAM_DECL;
    } else {
        result.kind = Flat_Node_Kind::FIELD_DECL;
    }
    result.scope = scope;
    result.param = param;
    return result;
}

Flat_Node to_flat_proto(AST_Declaration *decl)
{
    assert(decl);
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::FUNCTION_PROTO;
    result.decl = decl;

    return result;
}

bool name_resolve_node(Zodiac_Context *ctx, Flat_Node *node)
{
    assert(node);

    switch (node->kind) {
        case Flat_Node_Kind::DECL: {
            return name_resolve_decl(node->decl, node->scope);
        }

        case Flat_Node_Kind::STMT: {
            return name_resolve_stmt(node->stmt, node->scope);
        }

        case Flat_Node_Kind::EXPR: {
            return name_resolve_expr(ctx, node->expr, node->scope);
        }

        case Flat_Node_Kind::TS: {
            return name_resolve_ts(ctx, node->ts, node->scope);
        }

        case Flat_Node_Kind::PARAM_DECL: {
            Symbol *param_sym = scope_get_symbol(node->scope, node->param.identifier);
            assert(param_sym);
            assert(param_sym->decl->kind == AST_Declaration_Kind::FUNCTION);

            assert(param_sym->state == Symbol_State::UNRESOLVED);
            param_sym->state = Symbol_State::RESOLVED;
            return true;
        }

        case Flat_Node_Kind::FIELD_DECL: {
            Symbol *field_sym = scope_get_symbol(node->scope, node->field.identifier);
            assert(field_sym);
            assert(field_sym->decl->kind == AST_Declaration_Kind::STRUCT ||
                   field_sym->decl->kind == AST_Declaration_Kind::UNION);

            assert(field_sym->state == Symbol_State::UNRESOLVED);
            field_sym->state = Symbol_State::RESOLVED;
            return true;
        }

        case Flat_Node_Kind::FUNCTION_PROTO: {
            // The types and function name have been handled before
            return true;
        }
    }

    assert(false);
    return false;
}

bool name_resolve_decl(AST_Declaration *decl, Scope *scope)
{
    assert(decl && scope);

    assert(decl->identifier.name.data);
    auto decl_sym = scope_get_symbol(scope, decl->identifier.name);

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

        case Symbol_State::RESOLVED: {
            return true;
        }

        case Symbol_State::TYPED: assert(false);
    }

    bool result = true;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE:
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

        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            // Leaf, optional value should have been resolved already
            break;
        }

        case AST_Statement_Kind::PRINT: {
            // Leaf, value should have ben resolved already
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
        case AST_Expression_Kind::BINARY: {
            // Leaf
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
                assert(sym->state == Symbol_State::RESOLVED);
            }
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
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
            return type_resolve_declaration(node->decl, node->scope);
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

        case Flat_Node_Kind::PARAM_DECL: assert(false);
        case Flat_Node_Kind::FIELD_DECL: assert(false);

        case Flat_Node_Kind::FUNCTION_PROTO: {
            AST_Declaration *func_decl = node->decl;
            assert(func_decl->kind == AST_Declaration_Kind::FUNCTION);
            assert(func_decl->function.type == nullptr);

            if (func_decl->function.return_ts) {
                assert(func_decl->function.return_ts->resolved_type);
            }

            Dynamic_Array<Type *> param_types;
            dynamic_array_create(&ctx->temp_allocator, &param_types, func_decl->function.params.count);
            auto mark = temporary_allocator_get_mark(&ctx->temp_allocator_state);

            for (u64 i = 0; i < func_decl->function.params.count; i++) {
                AST_Field_Declaration *param_field = &func_decl->function.params[i];

                assert(param_field->type_spec);
                assert(param_field->type_spec->resolved_type);
            }

            // TODO: At this point we can create a function type
            func_decl->function.type = get_function_type(func_decl->function.return_ts->resolved_type, param_types, &ctx->ast_allocator);

            temporary_allocator_reset(&ctx->temp_allocator_state, mark);
            return true;
        }

    }

    assert(false);
    return false;
}

bool type_resolve_declaration(AST_Declaration *decl, Scope *scope)
{
    assert(decl);
    assert(scope);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {
            assert(decl->variable.resolved_type == nullptr);

            if (decl->variable.type_spec && decl->variable.value) {
                assert(valid_static_type_conversion(decl->variable.value->resolved_type,
                                                    decl->variable.type_spec->resolved_type));
                decl->variable.resolved_type = decl->variable.type_spec->resolved_type;
            } else {
                assert(decl->variable.type_spec);
                decl->variable.resolved_type = decl->variable.type_spec->resolved_type;
            }

            assert(decl->variable.resolved_type);

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            assert(decl->variable.resolved_type == nullptr);
            assert(decl->variable.value);

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

        case AST_Declaration_Kind::FUNCTION: {
            assert(decl->function.type);
            assert(decl->function.type->kind == Type_Kind::FUNCTION);
            return true;
        }

        case AST_Declaration_Kind::STRUCT: assert(false);
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
        case AST_Statement_Kind::BLOCK: assert(false);
        case AST_Statement_Kind::DECLARATION: assert(false);
        case AST_Statement_Kind::ASSIGN: assert(false);
        case AST_Statement_Kind::CALL: assert(false);
        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            AST_Declaration *fn_decl = enclosing_function(scope);
            assert(fn_decl && fn_decl->kind == AST_Declaration_Kind::FUNCTION);

            //TODO: Use the function proto here
            // This should be set when the function proto is resolved/typed
            assert(fn_decl->function.type);
            assert(fn_decl->function.type->kind == Type_Kind::FUNCTION);

            if (stmt->return_stmt.value) {

                auto actual_type = stmt->return_stmt.value->resolved_type;
                auto expected_type = fn_decl->function.return_ts->resolved_type;

                assert(actual_type && expected_type);

                if (actual_type == expected_type) return true;

                bool valid_conversion = valid_static_type_conversion(actual_type, expected_type);
                if (!valid_conversion) {
                    assert(false); // report error
                    return false;
                }

            } else {
                assert(false) // return void;
            }

            return true;
        }

        case AST_Statement_Kind::PRINT: assert(false);
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
            expr->resolved_type = &builtin_type_unsized_integer;
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false);
        case AST_Expression_Kind::NULL_LITERAL: assert(false);

        case AST_Expression_Kind::IDENTIFIER: {
            auto sym = scope_get_symbol(scope, expr->identifier.name);
            assert(sym);
            if (sym->state != Symbol_State::TYPED) {
                resolve_error(ctx, expr, "Waiting for declaration to be resolved");
                return false;
            }

            expr->resolved_type = decl_type(sym->decl);

            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);
        case AST_Expression_Kind::CALL: assert(false);
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
                    } else {
                        assert(false); // Report error
                    }

                } else if (lhs->resolved_type->kind == Type_Kind::UNSIZED_INTEGER &&
                           rhs->resolved_type->kind == Type_Kind::INTEGER) {

                    if (valid_static_type_conversion(lhs->resolved_type, rhs->resolved_type)) {
                        expr->resolved_type= rhs->resolved_type;
                    } else {
                        assert(false); // Report error
                    }

                } else {
                    assert(false);
                }
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
                assert(false);
            } else {
                assert((sym->flags & SYM_FLAG_BUILTIN) == SYM_FLAG_BUILTIN)
                assert(sym->builtin_type);
                ts->resolved_type = sym->builtin_type;
                return true;
            }
        }

        case AST_Type_Spec_Kind::POINTER: assert(false);
    }

    assert(false);
    return false;
}

}
