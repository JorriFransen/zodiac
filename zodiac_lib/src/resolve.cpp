#include "resolve.h"

#include "ast.h"
#include "atom.h"
#include "error.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/zstring.h"
#include "zodiac_context.h"

#include <stdio.h>

namespace Zodiac
{

void resolver_create(Resolver *resolver, Zodiac_Context *ctx)
{
    debug_assert(resolver);
    debug_assert(ctx);

    resolver->ctx = ctx;
    resolver->global_scope = scope_new(&ctx->ast_allocator, Scope_Kind::GLOBAL, nullptr);
    resolver->node_allocator = &ctx->ast_allocator;

    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_name_resolve);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_type_resolve);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_emit_bytecode);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_run_bytecode);
}

void resolver_destroy(Resolver *resolver)
{
    dynamic_array_free(&resolver->nodes_to_name_resolve);
    dynamic_array_free(&resolver->nodes_to_type_resolve);
    dynamic_array_free(&resolver->nodes_to_emit_bytecode);
    dynamic_array_free(&resolver->nodes_to_run_bytecode);
}

#define add_builtin_type_symbol(type)                                                                                                \
{                                                                                                                                    \
    auto sym = add_typed_symbol(resolver->ctx, resolver->global_scope, Symbol_Kind::TYPE, (SYM_FLAG_BUILTIN), atom_##type, nullptr); \
    sym->builtin_type = &builtin_type_##type;                                                                                        \
}

void resolver_add_file(Resolver *resolver, AST_File *file)
{
    debug_assert(file);

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
        auto decl = file->declarations[i];
        if (!add_unresolved_decl_symbol(resolver->ctx, resolver->global_scope, decl, true)) {
            assert(resolver->ctx->fatal_resolve_error);
            // assert(false);
            return;
        }
    }

    // Flatten all top level declarations, registering all nested symbols in the process
    for (s64 i = 0; i < file->declarations.count; i++) {
        resolver_add_declaration(resolver->ctx, resolver, file->declarations[i], resolver->global_scope);
    }
}

bool resolver_cycle(Resolver *resolver)
{
    debug_assert(resolver);

    bool done = false;

    Resolve_Results names_result = resolve_names(resolver);

    if (resolver->ctx->fatal_resolve_error) return false;;

    Resolve_Results types_result = resolve_types(resolver);

    if (resolver->ctx->fatal_resolve_error) return false;

    done = ((names_result & RESOLVE_RESULT_DONE) == RESOLVE_RESULT_DONE) &&
           ((types_result & RESOLVE_RESULT_DONE) == RESOLVE_RESULT_DONE);

    bool progress = ((names_result & RESOLVE_RESULT_PROGRESS) == RESOLVE_RESULT_PROGRESS) ||
                    ((types_result & RESOLVE_RESULT_PROGRESS) == RESOLVE_RESULT_PROGRESS);

    if (done) assert(resolver->ctx->errors.count == 0);

    if (!progress && !done) {
        assert(resolver->ctx->errors.count);
        return false;
    } else if (progress && resolve_error_count(resolver->ctx)) {
        resolver->ctx->errors.count = 0;
        temporary_allocator_reset(&resolver->ctx->error_allocator_state);
    }

    return done;
}

bool resolver_report_errors(Resolver *resolver)
{
    debug_assert(resolver);

    auto c = resolver->ctx;

    bool errors = false;

    for (s64 i = 0; i < c->errors.count; i++) {

        auto err = c->errors[i];

        if (!c->fatal_resolve_error) assert(!err.fatal);

        bool print = c->fatal_resolve_error == err.fatal;

        if (print) {
            auto start = err.source_range.start;
            printf("%s:%llu:%llu: error: %s\n", start.name.data, start.line, start.index_in_line, err.message.data);
            errors = true;
        }
    }

    return errors;
}

void resolver_add_declaration(Zodiac_Context *ctx, Resolver *resolver, AST_Declaration *decl, Scope *scope)
{
    debug_assert(resolver);
    debug_assert(decl);
    debug_assert(scope);

    auto node = alloc<Flat_Root_Node>(resolver->node_allocator);
    node->root.kind = Flat_Node_Kind::DECL;
    node->root.decl = decl;
    node->name_index = 0;
    node->type_index = 0;
    dynamic_array_create(resolver->node_allocator, &node->nodes);

    if (decl->kind == AST_Declaration_Kind::FUNCTION) {
        auto proto_node = alloc<Flat_Root_Node>(resolver->node_allocator);
        proto_node->root.kind = Flat_Node_Kind::FUNCTION_PROTO;
        proto_node->root.decl = decl;
        proto_node->name_index = 0;
        proto_node->type_index = 0;
        dynamic_array_create(&dynamic_allocator, &proto_node->nodes);

        for (s64 i = 0; i < decl->function.params.count; i++) {
            auto field = decl->function.params[i];
            assert(field->kind == AST_Declaration_Kind::PARAMETER);

            flatten_type_spec(field->field.type_spec, scope, &proto_node->nodes);
        }

        if (decl->function.return_ts) {
            flatten_type_spec(decl->function.return_ts, scope, &proto_node->nodes);
        }

        Flat_Node flat_decl = to_flat_proto(decl, scope);
        dynamic_array_append(&proto_node->nodes, flat_decl);

        dynamic_array_append(&resolver->nodes_to_name_resolve, proto_node);
        dynamic_array_append(&resolver->nodes_to_type_resolve, proto_node);
    }

    flatten_declaration(resolver, decl, resolver->global_scope, &node->nodes);

    dynamic_array_append(&resolver->nodes_to_name_resolve, node);
    dynamic_array_append(&resolver->nodes_to_type_resolve, node);
}

Resolve_Results resolve_names(Resolver *resolver)
{
    bool progress = false;
    bool done = true;

    for (u64 flat_index = 0; flat_index < resolver->nodes_to_name_resolve.count; flat_index++)
    {

        Flat_Root_Node *node = resolver->nodes_to_name_resolve[flat_index];

        for (u64 i = node->name_index; i < node->nodes.count; i++)
        {

            node->name_index = i;
            bool result = name_resolve_node(resolver, &node->nodes[i]);

            if (!result)
            {
                done = false;
                break;
            }
            else
            {
                node->name_index += 1;
                progress = true;
            }
        }

        if (node->name_index >= node->nodes.count)
        {
            // Done name resolving, move to type resolving
            // node->name_index = 0;

            // auto node_copy = *node;

            dynamic_array_remove_ordered(&resolver->nodes_to_name_resolve, flat_index);
            flat_index -= 1;

            // dynamic_array_append(&resolver->nodes_to_type_resolve, node_copy);
        }
    }

    Resolve_Results result = RESOLVE_RESULT_NONE;
    if (done) result |= RESOLVE_RESULT_DONE;
    if (progress) result |= RESOLVE_RESULT_PROGRESS;

    return result;
}

Resolve_Results resolve_types(Resolver *resolver)
{
    bool progress = false;
    bool done = true;

    for (u64 flat_index = 0; flat_index < resolver->nodes_to_type_resolve.count; flat_index++) {

        Flat_Root_Node *node = resolver->nodes_to_type_resolve[flat_index];

        for (u64 i = node->type_index; i < node->nodes.count; i++) {

            if (node->type_index >= node->name_index) {
                done = false;
                break;
            }

            node->type_index = i;
            bool result = type_resolve_node(resolver->ctx, &node->nodes[i]);

            if (!result) {
                done = false;
                break;
            } else {
                node->type_index += 1;
                progress = true;
            }
        }

        if (node->type_index >= node->nodes.count) {
            // Done type resolving, move to bytecode emit
            // node->current_index = 0;

            dynamic_array_remove_ordered(&resolver->nodes_to_type_resolve, flat_index);
            flat_index -= 1;

            dynamic_array_append(&resolver->nodes_to_emit_bytecode, node);
        }
    }

    Resolve_Results result = RESOLVE_RESULT_NONE;
    if (done)
        result |= RESOLVE_RESULT_DONE;
    if (progress)
        result |= RESOLVE_RESULT_PROGRESS;

    return result;
}

Infer_Node *infer_node_new(Zodiac_Context *ctx, Infer_Source source, Infer_Target target)
{
    debug_assert(ctx);

    auto result = alloc<Infer_Node>(&ctx->ast_allocator);
    result->source_kind = source;
    result->target_kind = target;
    return result;
}

Infer_Node *infer_node_new(Zodiac_Context *ctx, AST_Type_Spec *ts)
{
    debug_assert(ctx && ts);

    auto result = infer_node_new(ctx, Infer_Source::TYPE_SPEC, Infer_Target::DEFAULT);
    result->source.type_spec = ts;

    return result;
}

Infer_Node *infer_node_new(Zodiac_Context *ctx, Type *type)
{
    debug_assert(ctx && type);

    auto result = infer_node_new(ctx, Infer_Source::TYPE, Infer_Target::DEFAULT);
    result->source.type = type;

    return result;
}

Infer_Node *arg_infer_node_new(Zodiac_Context *ctx, AST_Expression *call_base_expr, s64 arg_index)
{
    debug_assert(ctx && call_base_expr);
    assert(arg_index >= 0);

    auto result = infer_node_new(ctx, Infer_Source::EXPR, Infer_Target::ARGUMENT);
    result->source.expr = call_base_expr;
    result->target.index = arg_index;

    return result;
}

Infer_Node *member_infer_node_new(Zodiac_Context *ctx, AST_Type_Spec *ag_ts, s64 member_index)
{
    debug_assert(ctx && ag_ts);
    assert(member_index >= 0);

    auto result = infer_node_new(ctx, Infer_Source::TYPE_SPEC, Infer_Target::MEMBER);
    result->source.type_spec = ag_ts;
    result->target.index = member_index;

    return result;
}

void flatten_declaration(Resolver *resolver, AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    debug_assert(resolver && decl && scope && dest);

    auto ctx = resolver->ctx;

    Scope *aggregate_scope = nullptr;
    Scope *parameter_scope = nullptr;
    Scope *local_scope = nullptr;

    if (decl->kind != AST_Declaration_Kind::RUN_DIRECTIVE) {

        assert(decl->identifier.name.data);
        auto decl_sym = scope_get_symbol(scope, decl->identifier.name);
        if (decl_sym && decl_sym->decl != decl)
        {
            report_redecl(ctx, decl_sym->range, decl->identifier.name, decl->identifier.range);
            return;
        }

        if (scope->kind == Scope_Kind::GLOBAL && !decl_sym) {

            assert_msg(decl_sym, "Global symbol should have been registered already");

        } else if (!decl_sym) { // First time local symbol is encountered

            if (!add_unresolved_decl_symbol(ctx, scope, decl, false)) {
                return;
            }
            decl_sym = scope_get_symbol(scope, decl->identifier);
        }

        assert(decl_sym && decl_sym->decl == decl);

        switch (decl_sym->kind) {

            default:
                break;

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

    }

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID:
            assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE:
        case AST_Declaration_Kind::PARAMETER:
        case AST_Declaration_Kind::FIELD: {
            auto ts = decl->variable.type_spec;
            auto val = decl->variable.value;

            Infer_Node *infer_from = nullptr;

            if (ts) {
                flatten_type_spec(ts, scope, dest);
                infer_from = infer_node_new(resolver->ctx, ts);
            }
            if (val)
                flatten_expression(resolver, val, scope, dest, infer_from);

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
                flatten_statement(resolver, decl->function.body[i], local_scope, dest);
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

        case AST_Declaration_Kind::RUN_DIRECTIVE: {
            flatten_directive(resolver, decl->directive, scope, dest);
            break;
        }
    }

    Flat_Node flat_decl = to_flat_node(decl, scope);
    dynamic_array_append(dest, flat_decl);
}

void flatten_statement(Resolver *resolver, AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    debug_assert(resolver && stmt && scope && dest);

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {

            Scope *block_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
            stmt->block.scope = block_scope;

            for (u64 i = 0; i < stmt->block.statements.count; i++) {
                flatten_statement(resolver, stmt->block.statements[i], block_scope, dest);
            }
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            flatten_declaration(resolver, stmt->decl.decl, scope, dest);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            flatten_expression(resolver, stmt->assign.dest, scope, dest);
            flatten_expression(resolver, stmt->assign.value, scope, dest);
            break;
        }

        case AST_Statement_Kind::CALL: {
            flatten_expression(resolver, stmt->call.call, scope, dest);
            break;
        }

        case AST_Statement_Kind::IF: {

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = &stmt->if_stmt.blocks[i];

                flatten_expression(resolver, if_block->cond, scope, dest);

                auto then_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
                assert(if_block->then_scope == nullptr);
                if_block->then_scope = then_scope;
                flatten_statement(resolver, if_block->then, then_scope, dest);
            }

            if (stmt->if_stmt.else_stmt) {
                auto else_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
                flatten_statement(resolver, stmt->if_stmt.else_stmt, else_scope, dest);
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

                Infer_Node *infer_from = nullptr;

                if (func_decl->function.return_ts) {
                    infer_from = infer_node_new(resolver->ctx, func_decl->function.return_ts);
                }

                stmt->return_stmt.scope = scope;

                flatten_expression(resolver, stmt->return_stmt.value, scope, dest, infer_from);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            for (s64 i = 0; i < stmt->print_expr.expressions.count; i++) {
                flatten_expression(resolver, stmt->print_expr.expressions[i], scope, dest);
            }
            break;
        }
    }

    Flat_Node flat_stmt = to_flat_node(stmt, scope);
    dynamic_array_append(dest, flat_stmt);
}

void flatten_expression(Resolver *resolver, AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest, Infer_Node *infer_node/*=nullptr*/)
{
    debug_assert(resolver && expr && scope && dest);

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID:
            assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::REAL_LITERAL:
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
            flatten_expression(resolver, expr->member.base, scope, dest);
            break;
        }

        case AST_Expression_Kind::INDEX:
            assert(false);

        case AST_Expression_Kind::CALL: {
            flatten_expression(resolver, expr->call.base, scope, dest);

            for (s64 i = 0; i < expr->call.args.count; i++) {
                Infer_Node *arg_infer_node = arg_infer_node_new(resolver->ctx, expr->call.base, i);
                flatten_expression(resolver, expr->call.args[i], scope, dest, arg_infer_node);
            }
            break;
        }

        case AST_Expression_Kind::UNARY:
            assert(false);

        case AST_Expression_Kind::BINARY: {
            // assert(infer_type_from);

            Infer_Node *infer_from = infer_node;
            if (expr->binary.lhs->kind == AST_Expression_Kind::INTEGER_LITERAL) {
                infer_from = nullptr;
            }

            flatten_expression(resolver, expr->binary.lhs, scope, dest, infer_from);

            infer_from = infer_node;
            if (expr->binary.rhs->kind == AST_Expression_Kind::INTEGER_LITERAL) {
                infer_from = nullptr;
            }

            flatten_expression(resolver, expr->binary.rhs, scope, dest, infer_from);
            break;
        }


        case AST_Expression_Kind::CAST: {
            assert(expr->cast.type_spec);
            flatten_type_spec(expr->cast.type_spec, scope, dest);
            flatten_expression(resolver, expr->cast.value, scope, dest);
            break;
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            flatten_directive(resolver, expr->directive.directive, scope, dest);
            assert(!expr->directive.directive->run.scope);
            expr->directive.directive->run.scope = scope;
            break;
        }

        case AST_Expression_Kind::COMPOUND: {

            assert(infer_node->source_kind == Infer_Source::TYPE_SPEC);
            assert(infer_node->target_kind == Infer_Target::DEFAULT);

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {
                Infer_Node *infer_from = member_infer_node_new(resolver->ctx, infer_node->source.type_spec, i);
                flatten_expression(resolver, expr->compound.expressions[i], scope, dest, infer_from);
            }
        }

    }

    Flat_Node flat_expr = to_flat_node(expr, scope, infer_node);
    dynamic_array_append(dest, flat_expr);
}

void flatten_type_spec(AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    debug_assert(ts && scope && dest);

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID:
            assert(false);

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

void flatten_directive(Resolver *resolver, AST_Directive *directive, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    debug_assert(resolver && directive && scope && dest);
    debug_assert(directive->kind == AST_Directive_Kind::RUN);

    switch (directive->run.kind) {

        case AST_Run_Directive_Kind::INVALID: assert(false); break;

        case AST_Run_Directive_Kind::EXPR: {

            flatten_expression(resolver, directive->run.expr, scope, dest);
            break;
        }

        case AST_Run_Directive_Kind::STMT: {
            flatten_statement(resolver, directive->run.stmt, scope, dest);
            break;
        }
    }
}

Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope)
{
    debug_assert(decl && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::DECL;
    result.scope = scope;
    result.decl = decl;
    return result;
}

Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope)
{
    debug_assert(stmt && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::STMT;
    result.scope = scope;
    result.stmt = stmt;
    return result;
}

Flat_Node to_flat_node(AST_Expression *expr, Scope *scope, Infer_Node *infer_type_from/*nullptr*/)
{
    debug_assert(expr && scope);

    Flat_Node result = {};
    result.scope = scope;
    result.kind = Flat_Node_Kind::EXPR;
    result.expr.expr = expr;
    result.expr.infer_type_from = infer_type_from;
    return result;
}

Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope)
{
    debug_assert(ts && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TYPE_SPEC;
    result.scope = scope;
    result.ts = ts;
    return result;
}

Flat_Node to_flat_proto(AST_Declaration *decl, Scope *scope)
{
    debug_assert(decl && decl->kind == AST_Declaration_Kind::FUNCTION);
    debug_assert(scope && scope->kind == Scope_Kind::GLOBAL);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::FUNCTION_PROTO;
    result.decl = decl;
    result.scope = scope;

    return result;
}

bool name_resolve_node(Resolver *resolver, Flat_Node *node)
{
    debug_assert(resolver && node);

    switch (node->kind)
    {
        case Flat_Node_Kind::DECL: {
            return name_resolve_decl(resolver, node->decl, node->scope);
        }

        case Flat_Node_Kind::STMT: {
            return name_resolve_stmt(resolver->ctx, node->stmt, node->scope);
        }

        case Flat_Node_Kind::EXPR: {
            return name_resolve_expr(resolver->ctx, node->expr.expr, node->scope);
        }

        case Flat_Node_Kind::TYPE_SPEC: {
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
    debug_assert(resolver);
    debug_assert(decl && scope);

    bool global = DECL_IS_GLOBAL(decl);
    Symbol *decl_sym = nullptr;

    if (decl->kind != AST_Declaration_Kind::RUN_DIRECTIVE) {

        assert(decl->identifier.name.data);
        decl_sym = scope_get_symbol(scope, decl->identifier.name);

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

            case Symbol_State::RESOLVING:
                assert_msg(false, "Circular dependency");

            case Symbol_State::RESOLVED:
            case Symbol_State::TYPED: {
                return true;
            }
        }

    }

    bool result = true;

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {

            assert(decl_sym);

            if (global) {

                auto init_expr = decl->variable.value;
                if (init_expr && !EXPR_IS_CONST(init_expr) && init_expr->kind != AST_Expression_Kind::RUN_DIRECTIVE) {
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

        case AST_Declaration_Kind::RUN_DIRECTIVE: {
            return true;
        }
    }

    if (decl_sym) {
        decl_sym = scope_get_symbol(scope, decl->identifier.name);
        assert(decl_sym);
        if (result) {
            decl_sym->state = Symbol_State::RESOLVED;
        } else {
            decl_sym->state = Symbol_State::UNRESOLVED;
        }
    }

    return result;
}

bool name_resolve_stmt(Zodiac_Context *ctx, AST_Statement *stmt, Scope *scope)
{
    debug_assert(ctx && stmt && scope);

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
            break;
        }

        case AST_Statement_Kind::CALL: {
            // Leaf, base expr and args should have been resolved already
            break;
        }

        case AST_Statement_Kind::IF: {
            break; // all resolved
        }

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
    debug_assert(expr && scope);

    bool result = true;

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID:
            assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::REAL_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL:
        case AST_Expression_Kind::RUN_DIRECTIVE: {
            debug_assert(EXPR_IS_CONST(expr));
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

            AST_Expression *base_expr = expr->member.base;
            if (!base_expr->resolved_type) {
                resolve_error(ctx, expr, "Waiting for base to be resolved");
                return false;
            }

            auto aggregate_type = base_expr->resolved_type;
            assert((aggregate_type->flags & TYPE_FLAG_AGGREGATE) == TYPE_FLAG_AGGREGATE);

            assert(aggregate_type->structure.name.length);

            auto type_sym = scope_get_symbol(scope, aggregate_type->structure.name);
            assert(type_sym);
            assert(type_sym->kind == Symbol_Kind::TYPE);
            assert(type_sym->aggregate.scope);
            assert(type_sym->aggregate.scope->kind == Scope_Kind::AGGREGATE);

            s64 member_index;
            auto sym = scope_get_symbol_direct(type_sym->aggregate.scope, expr->member.member_name, &member_index);
            if (!sym) {
                assert(type_sym->decl);
                fatal_resolve_error(ctx, expr, "'%s' is not a member of aggregate type '%s'", expr->member.member_name.data, aggregate_type->structure.name);
                fatal_resolve_error(ctx, type_sym->decl, "'%s' was declared here", aggregate_type->structure.name);
                result = false;
                break;
            }
            assert(sym);
            assert(sym->kind == Symbol_Kind::MEMBER);
            assert(sym->state == Symbol_State::TYPED);
            assert(member_index >= 0 && member_index < aggregate_type->structure.member_types.count);
            expr->member.index_in_parent = member_index;

            break;
        }

        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::COMPOUND: {
            // leaf
            break;
        }

    }

    return result;
}

bool name_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope)
{
    debug_assert(ts && scope);

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
                assert(sym->state >= Symbol_State::RESOLVED);
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
    debug_assert(ctx && node);

    switch (node->kind) {

        case Flat_Node_Kind::DECL: {
            return type_resolve_declaration(ctx, node->decl, node->scope);
            break;
        }

        case Flat_Node_Kind::STMT: {
            return type_resolve_statement(ctx, node->stmt, node->scope);
            break;
        }

        case Flat_Node_Kind::EXPR: {
            return type_resolve_expression(ctx, node->expr.expr, node->scope, node->expr.infer_type_from);
            break;
        }

        case Flat_Node_Kind::TYPE_SPEC: {
            return type_resolve_ts(ctx, node->ts, node->scope);
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
    debug_assert(decl);
    debug_assert(scope);

    bool result = true;

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
                assert(decl->variable.type_spec && decl->variable.type_spec->resolved_type);
                decl->variable.resolved_type = decl->variable.type_spec->resolved_type;
            }

            assert(decl->variable.resolved_type);

            if (decl->variable.value && scope->kind == Scope_Kind::GLOBAL) {
                assert(EXPR_IS_CONST(decl->variable.value) || decl->variable.value->kind == AST_Expression_Kind::RUN_DIRECTIVE);
            }

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;

            result = true;
            break;
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

            result = true;
            break;
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

            result = true;
            break;
        }

        case AST_Declaration_Kind::FIELD: {
            assert(decl->field.resolved_type == nullptr);
            assert(decl->field.value == nullptr);
            assert(decl->field.type_spec);
            assert(decl->field.type_spec->resolved_type);

            decl->field.resolved_type = decl->field.type_spec->resolved_type;

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            assert(sym->kind == Symbol_Kind::MEMBER);
            sym->state = Symbol_State::TYPED;

            result = true;
            break;
        }

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

            result = decl->function.type != nullptr;
            break;
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

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            assert(sym->kind == Symbol_Kind::TYPE);
            sym->state = Symbol_State::TYPED;

            result = true;
            break;
        }

        case AST_Declaration_Kind::UNION: assert(false);

        case AST_Declaration_Kind::RUN_DIRECTIVE: {
            assert(EXPR_IS_TYPED(decl->directive->run.expr));
            result = true;
            break;
        }
    }

    if (result) {
        decl->flags |= AST_DECL_FLAG_TYPED;
    }

    return result;
}

bool type_resolve_statement(Zodiac_Context *ctx, AST_Statement *stmt, Scope *scope)
{
    debug_assert(ctx && stmt && scope);

    bool result = true;

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {
            // leaf
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            // This declaration should have been emitted before the statement, and therefore resolved if we got to this point.
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            AST_Expression *lvalue_expr = stmt->assign.dest;
            AST_Expression *value_expr = stmt->assign.value;

            if (!EXPR_IS_LVALUE(lvalue_expr)) {
                fatal_resolve_error(ctx, lvalue_expr, "Left side of assignment must be an lvalue");
                result = false;
                break;
            }

            assert(lvalue_expr->resolved_type);
            assert(value_expr->resolved_type);

            if (value_expr->resolved_type != lvalue_expr->resolved_type &&
                valid_static_type_conversion(value_expr->resolved_type, lvalue_expr->resolved_type)) {
                value_expr->resolved_type = lvalue_expr->resolved_type;
            }

            break;
        }

        case AST_Statement_Kind::CALL: {
            // leaf
            break;
        }

        case AST_Statement_Kind::IF: {

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto &if_block = stmt->if_stmt.blocks[i];
                assert(if_block.cond->resolved_type);
                assert(if_block.cond->resolved_type->kind == Type_Kind::BOOLEAN);
            }

            break;
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
                        result = false;
                        break;
                    }

                    AST_Expression *value_expr = stmt->return_stmt.value;
                    AST_Expression *cast_expr = ast_cast_expr_new(ctx, value_expr->range, expected_type, value_expr);
                    stmt->return_stmt.value = cast_expr;

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


            break;
        }

        case AST_Statement_Kind::PRINT: {
            for (s64 i = 0; i < stmt->print_expr.expressions.count; i++) {
                auto type = stmt->print_expr.expressions[i]->resolved_type;
                assert(type);

                if (type->kind == Type_Kind::UNSIZED_INTEGER) {
                    type = &builtin_type_s64;
                    stmt->print_expr.expressions[i]->resolved_type = &builtin_type_s64;
                }

                assert(type->kind == Type_Kind::INTEGER ||
                       type->kind == Type_Kind::FLOAT   ||
                       type->kind == Type_Kind::BOOLEAN ||
                       type == &builtin_type_String);

            }

            break;
        }
    }

    if (result) {
        debug_assert(!STMT_IS_TYPED(stmt));
        stmt->flags |= AST_STMT_FLAG_TYPED;
    }

    return result;
}

bool type_resolve_expression(Zodiac_Context *ctx, AST_Expression *expr, Scope *scope, Infer_Node *infer_type_from)
{
    debug_assert(expr);
    debug_assert(expr->resolved_type == nullptr);
    debug_assert(scope);

    bool result = true;

    Type *inferred_type = nullptr;

    if (infer_type_from) {

        switch (infer_type_from->source_kind) {

            case Infer_Source::TYPE: inferred_type = infer_type_from->source.type; break;

            case Infer_Source::TYPE_SPEC: {
                auto ts = infer_type_from->source.type_spec;
                if (!ts->resolved_type) {
                    resolve_error(ctx, expr, "Waiting for type spec to be typed");
                    resolve_error(ctx, ts, "Type spec is here");
                    return false;
                }
                inferred_type = ts->resolved_type;
                break;
            }

            case Infer_Source::EXPR: {
                auto expr = infer_type_from->source.expr;
                assert(EXPR_IS_TYPED(expr));

                inferred_type = expr->resolved_type;
                break;
            }
        }

        assert(inferred_type);

        switch (infer_type_from->target_kind) {

            case Infer_Target::DEFAULT: break;

            case Infer_Target::ARGUMENT: {
                assert(inferred_type->kind == Type_Kind::FUNCTION);

                auto index = infer_type_from->target.index;
                assert(inferred_type->function.parameter_types.count > infer_type_from->target.index);

                inferred_type = inferred_type->function.parameter_types[index];
                break;
            }

            case Infer_Target::MEMBER: {
                assert(inferred_type->flags & TYPE_FLAG_AGGREGATE);
                assert(inferred_type->kind == Type_Kind::STRUCTURE);

                auto index = infer_type_from->target.index;
                assert(inferred_type->structure.member_types.count > infer_type_from->target.index);

                inferred_type = inferred_type->structure.member_types[index];
                break;
            }
        }

        assert(inferred_type);
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL: {

                if (inferred_type) {

                    if (inferred_type->kind != Type_Kind::INTEGER) {
                        fatal_resolve_error(ctx, expr, "Could not convert integer literal to inferred type '%s'",
                                                       temp_type_string(inferred_type).data);
                        return false;
                    }

                    // TODO: Make sure the literal fits in this type

                    expr->resolved_type = inferred_type;
                } else {
                    expr->resolved_type = &builtin_type_unsized_integer;
                }
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: {
            if (inferred_type) {
                assert(inferred_type->kind == Type_Kind::FLOAT);
                expr->resolved_type = inferred_type;
            } else {
                expr->resolved_type = &builtin_type_r32;
            }
            assert(expr->resolved_type);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            assert(!inferred_type);
            expr->resolved_type = &builtin_type_String;
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::BOOL_LITERAL: {
            if (inferred_type) assert(inferred_type->kind == Type_Kind::BOOLEAN);
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

            if (sym->kind == Symbol_Kind::VAR ||
                sym->kind == Symbol_Kind::PARAM) {
                expr->flags |= AST_EXPR_FLAG_LVALUE;
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: {

            auto base_expr = expr->member.base;
            assert(base_expr->resolved_type);

            auto aggregate_type = base_expr->resolved_type;
            assert((aggregate_type->flags & TYPE_FLAG_AGGREGATE) == TYPE_FLAG_AGGREGATE);

            auto type_sym = scope_get_symbol(scope, aggregate_type->structure.name);
            assert(type_sym);
            assert(type_sym->kind == Symbol_Kind::TYPE);

            assert(type_sym->aggregate.scope && type_sym->aggregate.scope->kind == Scope_Kind::AGGREGATE);
            auto sym = scope_get_symbol(type_sym->aggregate.scope, expr->member.member_name);
            assert(sym && sym->kind == Symbol_Kind::MEMBER);
            assert(sym->state == Symbol_State::TYPED);

            auto mem_decl = sym->decl;
            assert(mem_decl && mem_decl->kind == AST_Declaration_Kind::FIELD);
            assert(mem_decl->field.resolved_type);

            expr->resolved_type = mem_decl->field.resolved_type;

            assert(EXPR_IS_LVALUE(base_expr));
            expr->flags |= AST_EXPR_FLAG_LVALUE;
            break;
        }

        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {

            assert(expr->call.base->kind == AST_Expression_Kind::IDENTIFIER);
            auto ident_expr = expr->call.base;

            auto func_sym = scope_get_symbol(ident_expr->identifier.scope, ident_expr->identifier.name);
            assert(func_sym && func_sym->kind == Symbol_Kind::FUNC);
            assert(func_sym->state == Symbol_State::TYPED);

            if (!DECL_IS_TYPED(func_sym->decl)) {
                resolve_error(ctx, ident_expr, "Waiting for declaration to be typed");
                return false;
            }

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
                } else if (inferred_type){
                    assert(inferred_type->kind == Type_Kind::INTEGER);
                    expr->resolved_type = inferred_type;
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

        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            assert(expr->directive.directive->kind == AST_Directive_Kind::RUN);
            auto dir = expr->directive.directive;

            Type *type = nullptr;

            switch (dir->run.kind) {

                case AST_Run_Directive_Kind::INVALID: assert(false); break;
                case AST_Run_Directive_Kind::EXPR: type = dir->run.expr->resolved_type; break;
                case AST_Run_Directive_Kind::STMT: assert(false); break;
            }

            assert(type);
            expr->resolved_type = type;
            break;
        }

        case AST_Expression_Kind::COMPOUND: {
            assert(inferred_type);

            auto aggregate_type = inferred_type;
            assert(aggregate_type->flags & TYPE_FLAG_AGGREGATE);
            assert(aggregate_type->kind == Type_Kind::STRUCTURE);

            auto aggregate_member_count = aggregate_type->structure.member_types.count;
            auto compound_member_count = expr->compound.expressions.count;

            if (aggregate_member_count != compound_member_count) {
                fatal_resolve_error(ctx, expr, "Mismatching expression count, expected %i, got %i",  aggregate_member_count, compound_member_count);
                return false;
            }

            for (s64 i = 0; i < aggregate_member_count; i++) {
                Type *aggregate_member_type = aggregate_type->structure.member_types[i];
                auto compound_member_expr = expr->compound.expressions[i];
                Type *compound_member_type = compound_member_expr->resolved_type;
                assert(compound_member_type);

                if (aggregate_member_type != compound_member_type) {
                    fatal_resolve_error(ctx, compound_member_expr, "Mismatching type for compound member %i", i + 1);
                    fatal_resolve_error(ctx, compound_member_expr, "    Expected: %s", temp_type_string(aggregate_member_type));
                    fatal_resolve_error(ctx, compound_member_expr, "    Got: %s", temp_type_string(compound_member_type));
                    return false;
                }
            }

            expr->resolved_type = inferred_type;
            break;
        }
    }

    assert(result);
    assert(expr->resolved_type);

    if (result) {
        expr->flags |= AST_EXPR_FLAG_TYPED;
    }

    return result;
}

bool type_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope)
{
    debug_assert(ctx && ts && scope);

    if (ts->resolved_type) return true;

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            auto sym = scope_get_symbol(scope, ts->identifier.name);

            if (sym->state != Symbol_State::TYPED)
            {
                resolve_error(ctx, sym->decl, "Waiting for symbol to be typed");
                return false;
            }

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
