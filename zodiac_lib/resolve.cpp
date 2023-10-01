#include "resolve.h"

#include "ast.h"
#include "atom.h"
#include "common.h"
#include "constant_resolver.h"
#include "error.h"
#include "platform/filesystem.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

#include <stdio.h>

namespace Zodiac
{

#define add_builtin_type_symbol(type)                                                                                                \
{                                                                                                                                    \
    auto sym = add_typed_symbol(resolver->ctx, resolver->global_scope, Symbol_Kind::TYPE, (SYM_FLAG_BUILTIN), atom_##type, nullptr); \
    sym->builtin_type = &builtin_type_##type;                                                                                        \
}

void resolver_create(Resolver *resolver, Zodiac_Context *ctx)
{
    debug_assert(resolver);
    debug_assert(ctx);

    ctx->resolver = resolver;

    resolver->ctx = ctx;
    resolver->global_scope = scope_new(&ctx->ast_allocator, Scope_Kind::GLOBAL, nullptr);
    resolver->node_allocator = &ctx->ast_allocator;

    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_name_resolve);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_type_resolve);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_emit_bytecode);
    dynamic_array_create(&dynamic_allocator, &resolver->nodes_to_run_bytecode);

    assert(resolver->global_scope);
    assert(resolver->global_scope->global.file == nullptr);

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
}

void resolver_destroy(Resolver *resolver)
{
    dynamic_array_free(&resolver->nodes_to_name_resolve);
    dynamic_array_free(&resolver->nodes_to_type_resolve);
    dynamic_array_free(&resolver->nodes_to_emit_bytecode);
    dynamic_array_free(&resolver->nodes_to_run_bytecode);
}

void resolver_add_file(Resolver *resolver, AST_File *file)
{
    debug_assert(file);

    dynamic_array_append(&resolver->ctx->parsed_files, file);

    // TODO: Cleanup:
    if (!resolver->global_scope->global.file) {
        resolver->global_scope->global.file = file;
    }

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

    done = (names_result & RESOLVE_RESULT_DONE) && (types_result & RESOLVE_RESULT_DONE);
    bool progress = (names_result & RESOLVE_RESULT_PROGRESS) || (types_result & RESOLVE_RESULT_PROGRESS);

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

            flatten_type_spec(resolver, field->field.type_spec, scope, &proto_node->nodes);
        }

        if (decl->function.return_ts) {
            flatten_type_spec(resolver, decl->function.return_ts, scope, &proto_node->nodes);
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
            bool result = type_resolve_node(resolver, &node->nodes[i]);

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

Infer_Node *infer_node_new(Zodiac_Context *ctx, AST_Expression *expr)
{
    debug_assert(ctx && expr);

    auto result = infer_node_new(ctx, Infer_Source::EXPR, Infer_Target::DEFAULT);
    result->source.expr = expr;

    return result;
}

Infer_Node *arg_infer_node_new(Zodiac_Context *ctx, Infer_Node *infer_node, s64 arg_index)
{
    debug_assert(ctx && infer_node);
    assert(arg_index >= 0);

    auto result = infer_node_new(ctx, Infer_Source::INFER_NODE, Infer_Target::ARGUMENT);
    result->source.infer_node = infer_node;
    result->target.index = arg_index;

    return result;
}

Infer_Node *compound_infer_node_new(Zodiac_Context *ctx, Infer_Node *infer_node, s64 member_index)
{
    debug_assert(ctx && infer_node);

    auto result = infer_node_new(ctx, Infer_Source::INFER_NODE, Infer_Target::COMPOUND);
    result->source.infer_node = infer_node;
    result->target.index = member_index;

    return result;
}

Type *infer_type(Zodiac_Context *ctx, Infer_Node *infer_node, Source_Range error_loc)
{
    debug_assert(ctx && infer_node);

    Type *inferred_type = nullptr;

    switch (infer_node->source_kind) {

        case Infer_Source::TYPE: inferred_type = infer_node->source.type; break;

        case Infer_Source::TYPE_SPEC: {
            auto ts = infer_node->source.type_spec;
            if (!ts->resolved_type) {
                resolve_error(ctx, error_loc, "Waiting for type spec to be typed");
                resolve_error(ctx, ts, "Type spec is here");
                return nullptr;
            }
            inferred_type = ts->resolved_type;
            break;
        }

        case Infer_Source::EXPR: {
            auto expr = infer_node->source.expr;
            if (!EXPR_IS_TYPED(expr)) {
                resolve_error(ctx, error_loc, "Waiting for expression to be typed");
                resolve_error(ctx, expr, "Expression is here");
                return nullptr;
            }

            inferred_type = expr->resolved_type;
            break;
        }

        case Infer_Source::INFER_NODE: {
            inferred_type = infer_type(ctx, infer_node->source.infer_node, error_loc);
            if (!inferred_type)
                return nullptr;
            break;
        }
    }

    assert(inferred_type);

    switch (infer_node->target_kind) {

        case Infer_Target::DEFAULT: break;

        case Infer_Target::ARGUMENT: {
            assert(inferred_type->kind == Type_Kind::FUNCTION);

            auto index = infer_node->target.index;
            assert(inferred_type->function.parameter_types.count > infer_node->target.index);

            inferred_type = inferred_type->function.parameter_types[index];
            break;
        }

        case Infer_Target::COMPOUND: {
            if (!((inferred_type->flags & TYPE_FLAG_AGGREGATE) ||
                   inferred_type->kind == Type_Kind::STATIC_ARRAY ||
                   inferred_type->kind == Type_Kind::SLICE)) {

                fatal_resolve_error(ctx, error_loc, "Invalid type for compound literal: '%s'", temp_type_string(inferred_type));
                return nullptr;
            }

            auto index = infer_node->target.index;

            if (inferred_type->kind == Type_Kind::STRUCTURE) {

                assert(inferred_type->structure.member_types.count > infer_node->target.index);
                inferred_type = inferred_type->structure.member_types[index];

            } else if (inferred_type->kind == Type_Kind::STATIC_ARRAY) {

                assert(index >= 0 && index < inferred_type->static_array.count);
                inferred_type = inferred_type->static_array.element_type;

            } else {
                assert(inferred_type->kind == Type_Kind::SLICE);
                inferred_type = inferred_type->slice.element_type;
            }

            assert(inferred_type);
            break;
        }
    }

    assert(inferred_type);

    return inferred_type;
}

void flatten_declaration(Resolver *resolver, AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    debug_assert(resolver && decl && scope && dest);

    auto ctx = resolver->ctx;

    Scope *aggregate_scope = nullptr;
    Scope *parameter_scope = nullptr;
    Scope *local_scope = nullptr;

    if (decl->kind != AST_Declaration_Kind::RUN_DIRECTIVE &&
        decl->kind != AST_Declaration_Kind::IMPORT_DIRECTIVE) {

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
                flatten_type_spec(resolver, ts, scope, dest);
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

                flatten_type_spec(resolver, field->parameter.type_spec, scope, dest);
                Flat_Node param_node = to_flat_node(field, parameter_scope);
                dynamic_array_append(dest, param_node);
            }

            if (decl->function.return_ts) {
                flatten_type_spec(resolver, decl->function.return_ts, scope, dest);
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

                flatten_type_spec(resolver, field->field.type_spec, scope, dest);
                Flat_Node member_node = to_flat_node(field, aggregate_scope);
                dynamic_array_append(dest, member_node);
            }

            break;
        }

        case AST_Declaration_Kind::RUN_DIRECTIVE: {
            flatten_directive(resolver, decl->directive, scope, dest);
            break;
        }

        case AST_Declaration_Kind::IMPORT_DIRECTIVE: {
            auto ta = temp_allocator_allocator();
            auto ta_mark = temporary_allocator_get_mark(temp_allocator());
            defer { temporary_allocator_reset(temp_allocator(), ta_mark); };

            Atom relative_path = decl->directive->import.path;

            Atom path;
            bool found = false;

            // Check the modules directory first
            {
                String test_path = string_format(ta, "%s" ZODIAC_PATH_SEPARATOR "%s", resolver->ctx->module_dir.data, relative_path.data);

                if (filesystem_is_regular(test_path)) {
                    path = atom_get(&resolver->ctx->atoms, test_path);
                    found = true;
                }
            }

            if (!found) {
                // Check relative to the file this #import is in
                String current_file_dir = filesystem_dir_name(ta, decl->range.start.name);
                assert(filesystem_is_dir(current_file_dir));

                String test_path = string_format(ta, "%s" ZODIAC_PATH_SEPARATOR "%s", current_file_dir.data, relative_path.data);

                if (string_equal(decl->range.start.name, test_path)) {
                    ZWARN("#import imports it's own file: '%s'", test_path.data);
                }

                if (filesystem_is_regular(test_path)) {
                    path = atom_get(&resolver->ctx->atoms, test_path);
                    found = true;
                }
            }

            if (!found) {
                fatal_resolve_error(ctx, decl, "Unable to find #import file: '%s'", relative_path.data);
                return;
            }

            bool already_imported = false;

            for (s64 i = 0; i < ctx->parsed_files.count; i++) {
                if (ctx->parsed_files[i]->name == path) {
                    already_imported = true;
                    break;
                }
            }
            for (s64 i = 0; i < ctx->files_to_parse.count; i++) {
                if (ctx->files_to_parse[i].path == path) {
                    already_imported = true;
                    break;
                }
            }

            if (already_imported) {
                break;
            }

            File_To_Parse ftp = {
                .kind = File_To_Parse_Kind::PATH,
                .path = path,
            };

            dynamic_array_append(&ctx->files_to_parse, ftp);

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
            Infer_Node *infer_node = infer_node_new(resolver->ctx, stmt->assign.dest);
            flatten_expression(resolver, stmt->assign.value, scope, dest, infer_node);
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

        case AST_Statement_Kind::WHILE: {

            auto while_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
            assert(!stmt->while_stmt.scope);
            stmt->while_stmt.scope = while_scope;

            flatten_expression(resolver, stmt->while_stmt.cond, while_scope, dest);

            flatten_statement(resolver, stmt->while_stmt.body_stmt, while_scope, dest);
            break;
        }


        case AST_Statement_Kind::FOR: {

            auto for_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, scope);
            assert(!stmt->for_stmt.scope);
            stmt->for_stmt.scope = for_scope;

            flatten_declaration(resolver, stmt->for_stmt.init_decl, for_scope, dest);
            flatten_expression(resolver, stmt->for_stmt.cond_expr, for_scope, dest);
            flatten_statement(resolver, stmt->for_stmt.inc_stmt, for_scope, dest);

            flatten_statement(resolver, stmt->for_stmt.body_stmt, for_scope, dest);
            break;
        }

        case AST_Statement_Kind::DEFER: {
            assert(scope->kind == Scope_Kind::FUNCTION_LOCAL);

            flatten_statement(resolver, stmt->defer_stmt.stmt, scope, dest);

            dynamic_array_append(&scope->func.defer_stmts, stmt);
            break;
        }

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
        case AST_Expression_Kind::CHAR_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL: {
            // Leaf expression
            break;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            if (expr->identifier.scope != nullptr) {
                assert(expr->identifier.scope == scope);
            }
            expr->identifier.scope = scope;
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            flatten_expression(resolver, expr->member.base, scope, dest);
            break;
        }

        case AST_Expression_Kind::INDEX: {
            flatten_expression(resolver, expr->index.base, scope, dest);

            auto index_expr = expr->index.index;
            assert(index_expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
                   index_expr->kind == AST_Expression_Kind::IDENTIFIER);

            Infer_Node *index_infer_node = nullptr;
            if (index_expr->kind == AST_Expression_Kind::INTEGER_LITERAL) {
                index_infer_node = infer_node_new(resolver->ctx, &builtin_type_s64);
            }

            flatten_expression(resolver, expr->index.index, scope, dest, index_infer_node);
            break;
        }

        case AST_Expression_Kind::CALL: {
            flatten_expression(resolver, expr->call.base, scope, dest);

            // We can't always use the passed in infer node, it might point
            //  to the return value type of the function. We need the function type here.
            auto fn_infer_node = infer_node_new(resolver->ctx, expr->call.base);

            for (s64 i = 0; i < expr->call.args.count; i++) {
                Infer_Node *arg_infer_node = arg_infer_node_new(resolver->ctx, fn_infer_node, i);
                flatten_expression(resolver, expr->call.args[i], scope, dest, arg_infer_node);
            }

            break;
        }

        case AST_Expression_Kind::UNARY: {
            flatten_expression(resolver, expr->unary.operand, scope, dest, infer_node);
            break;
        }

        case AST_Expression_Kind::BINARY: {
            if (expr->binary.lhs->kind == AST_Expression_Kind::NULL_LITERAL) {

                flatten_expression(resolver, expr->binary.rhs, scope, dest, infer_node);
                flatten_expression(resolver, expr->binary.lhs, scope, dest, infer_node_new(resolver->ctx, expr->binary.rhs));

            } else if (expr->binary.rhs->kind == AST_Expression_Kind::NULL_LITERAL) {

                flatten_expression(resolver, expr->binary.lhs, scope, dest, infer_node);
                flatten_expression(resolver, expr->binary.rhs, scope, dest, infer_node_new(resolver->ctx, expr->binary.lhs));

            } else {
                flatten_expression(resolver, expr->binary.lhs, scope, dest, infer_node);
                flatten_expression(resolver, expr->binary.rhs, scope, dest, infer_node);
            }
            break;
        }


        case AST_Expression_Kind::CAST: {
            assert(expr->cast.type_spec);
            flatten_type_spec(resolver, expr->cast.type_spec, scope, dest);
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

            if (!infer_node) {
                fatal_resolve_error(resolver->ctx, expr, "Could not infer type for compound literal");
                return;
            }

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {
                Infer_Node *infer_from = compound_infer_node_new(resolver->ctx, infer_node, i);
                flatten_expression(resolver, expr->compound.expressions[i], scope, dest, infer_from);
            }
        }

    }

    Flat_Node flat_expr = to_flat_node(expr, scope, infer_node);
    dynamic_array_append(dest, flat_expr);
}

void flatten_type_spec(Resolver *resolver, AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest, bool via_pointer/*=false*/)
{
    debug_assert(resolver && ts && scope && dest);

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID:
            assert(false);

        case AST_Type_Spec_Kind::TYPE:
        case AST_Type_Spec_Kind::NAME: {
            // Leaf
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            flatten_type_spec(resolver, ts->pointer_base, scope, dest, true);
            break;
        }

        case AST_Type_Spec_Kind::STATIC_ARRAY: {
            auto length_expr = ts->static_array.length_expr;

            assert(length_expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
                   length_expr->kind == AST_Expression_Kind::IDENTIFIER);

            Infer_Node *length_infer_node = nullptr;
            if (length_expr->kind == AST_Expression_Kind::INTEGER_LITERAL) {
                length_infer_node = infer_node_new(resolver->ctx, &builtin_type_s64);
            }

            flatten_expression(resolver, length_expr, scope, dest, length_infer_node);
            flatten_type_spec(resolver, ts->static_array.element_ts, scope, dest);
            break;
        }

        case AST_Type_Spec_Kind::SLICE: {
            flatten_type_spec(resolver, ts->slice.element_ts, scope, dest);
            break;
        }
    }

    Flat_Node flat_ts = to_flat_node(ts, scope, via_pointer);
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

Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope, bool via_pointer/*=false*/)
{
    debug_assert(ts && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TYPE_SPEC;
    result.scope = scope;
    result.type_spec.ts = ts;
    result.type_spec.via_pointer = via_pointer;
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
            return name_resolve_ts(resolver->ctx, node->type_spec.ts, node->scope, node->type_spec.via_pointer);
        }

        case Flat_Node_Kind::FUNCTION_PROTO: {
            // If we get here, we have name resolved all dependencies
            Symbol *sym = scope_get_symbol(node->scope, node->decl->identifier);
            assert(sym && sym->kind == Symbol_Kind::FUNC);
            assert(sym->state == Symbol_State::UNRESOLVED);
            sym->state = Symbol_State::RESOLVED;
            return true;
        }

        case Flat_Node_Kind::IMPLICIT_LVALUE: assert(false); break;

        case Flat_Node_Kind::RUN: assert(false); break;

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

    if (decl->kind != AST_Declaration_Kind::RUN_DIRECTIVE &&
        decl->kind != AST_Declaration_Kind::IMPORT_DIRECTIVE) {

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

        case AST_Declaration_Kind::IMPORT_DIRECTIVE: {
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

        case AST_Statement_Kind::IF:
        case AST_Statement_Kind::WHILE: {
            break; // all resolved
        }

        case AST_Statement_Kind::FOR: {
            if (stmt->for_stmt.init_decl->kind != AST_Declaration_Kind::VARIABLE) {
                fatal_resolve_error(ctx, stmt->for_stmt.init_decl, "Expected variable declaration in 'for'");
                result = false;
                break;
            }
            break;
        }

        case AST_Statement_Kind::DEFER: {
            // Leaf
            break;
        }

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
        case AST_Expression_Kind::CHAR_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL:
        case AST_Expression_Kind::RUN_DIRECTIVE: {
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

            if (aggregate_type->kind == Type_Kind::POINTER) {
                assert(aggregate_type->pointer.base->flags & TYPE_FLAG_AGGREGATE);
                aggregate_type = aggregate_type->pointer.base;
            } else if (aggregate_type->kind == Type_Kind::SLICE) {
                aggregate_type = aggregate_type->slice.struct_type;
            } else if (aggregate_type->kind == Type_Kind::STATIC_ARRAY) {
                if (expr->member.member_name != atom_get(&ctx->atoms, "length")) {
                    fatal_resolve_error(ctx, expr, "Static array does not have member: '%s'", expr->member.member_name.data);
                    result = false;
                }
                break;

            }

            assert(aggregate_type->flags & TYPE_FLAG_AGGREGATE);

            if (aggregate_type->flags & TYPE_FLAG_UNFINISHED_STRUCT_TYPE) {
                resolve_error(ctx, expr, "Waiting for struct type to be resolved");
                return false;
            }

            assert(aggregate_type->structure.name.length);

            auto type_sym = scope_get_symbol(scope, aggregate_type->structure.name);
            assert(type_sym);
            assert(type_sym->kind == Symbol_Kind::TYPE);
            assert(type_sym->aggregate.scope);
            assert(type_sym->aggregate.scope->kind == Scope_Kind::AGGREGATE);

            s64 member_index;
            auto sym = scope_get_symbol_direct(type_sym->aggregate.scope, expr->member.member_name, &member_index);
            if (!sym) {
                if (type_sym->decl) {
                    fatal_resolve_error(ctx, expr, "'%s' is not a member of aggregate type '%s'", expr->member.member_name.data, aggregate_type->structure.name.data);
                    fatal_resolve_error(ctx, type_sym->decl, "'%s' was declared here", aggregate_type->structure.name);
                    result = false;
                } else {
                    assert(type_sym->flags & SYM_FLAG_BUILTIN);
                    assert(type_sym->builtin_type);
                    if (type_sym->builtin_type->flags & TYPE_FLAG_SLICE_STRUCT) {
                        fatal_resolve_error(ctx, expr, "'%s' is not a member of type 'slice'", expr->member.member_name.data);
                        result = false;
                    } else {
                        assert(false);
                    }
                }
                break;
            }
            assert(sym);
            assert(sym->kind == Symbol_Kind::MEMBER);
            assert(sym->state == Symbol_State::TYPED);
            assert(member_index >= 0 && member_index < aggregate_type->structure.member_types.count);

            expr->member.index_in_parent = member_index;
            break;
        }

        case AST_Expression_Kind::UNARY: {
            if (EXPR_IS_CONST(expr->unary.operand)) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }
            break;
        }

        case AST_Expression_Kind::CAST: {
            // leaf
            break;
        }

        case AST_Expression_Kind::INDEX:
        case AST_Expression_Kind::COMPOUND: {
            // leaf
            break;
        }

    }

    return result;
}

bool name_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope, bool via_pointer)
{
    debug_assert(ts && scope);

    bool result = true;

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::TYPE: {
            assert(ts->resolved_type);
            return true;
        }

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

                if (!via_pointer) {
                    resolve_error(ctx, ts, "Unresolved symbol: '%s'", ts->identifier.name.data);
                    result = false;
                }
                break;

            } else {
                assert(sym->state >= Symbol_State::RESOLVED);
            }

            break;
        }

        case AST_Type_Spec_Kind::POINTER:
        case AST_Type_Spec_Kind::STATIC_ARRAY:
        case AST_Type_Spec_Kind::SLICE: {
            // Leaf
            break;
        }
    }

    return result;
}

bool type_resolve_node(Resolver *resolver, Flat_Node *node)
{
    debug_assert(resolver && node);

    switch (node->kind) {

        case Flat_Node_Kind::DECL: {
            return type_resolve_declaration(resolver->ctx, node->decl, node->scope);
            break;
        }

        case Flat_Node_Kind::STMT: {
            return type_resolve_statement(resolver, node->stmt, node->scope);
            break;
        }

        case Flat_Node_Kind::EXPR: {
            return type_resolve_expression(resolver, node->expr.expr, node->scope, node->expr.infer_type_from);
            break;
        }

        case Flat_Node_Kind::TYPE_SPEC: {
            return type_resolve_ts(resolver->ctx, node->type_spec.ts, node->scope, node->type_spec.via_pointer);
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
                    resolve_error(resolver->ctx, node->decl, "Could not infer return type");
                    return false;
                }

                auto param_types = temp_array_create<Type *>(temp_allocator_allocator(), func_decl->function.params.count);

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
                    fatal_resolve_error(resolver->ctx, func_decl, "Could not infer return type");
                    return false;
                }

                func_decl->function.type = get_function_type(return_type, param_types.array, &resolver->ctx->ast_allocator);

                temp_array_destroy(&param_types);
            }

            auto sym = scope_get_symbol(node->scope, node->decl->identifier.name);
            assert(sym && sym->kind == Symbol_Kind::FUNC);
            assert(sym->state == Symbol_State::RESOLVED);
            sym->state = Symbol_State::TYPED;
            return true;
        }

        case Flat_Node_Kind::IMPLICIT_LVALUE: assert(false); break;

        case Flat_Node_Kind::RUN: assert(false); break;
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

            if (decl->variable.value &&
                decl->variable.resolved_type->kind == Type_Kind::SLICE &&
                decl->variable.value->resolved_type->kind == Type_Kind::STATIC_ARRAY) {

                bool value_is_global_const = false;

                // Check if the value is pointing to a global (constant).
                if (decl->variable.value->kind == AST_Expression_Kind::IDENTIFIER) {
                    auto ident_sym = scope_get_symbol(scope, decl->variable.value->identifier.name);
                    auto ident_decl = ident_sym->decl;

                    if (ident_decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE) {
                        value_is_global_const = true;
                    }
                }

                bool needs_local_array_alloc = (decl->variable.value->kind == AST_Expression_Kind::COMPOUND || EXPR_IS_CONST(decl->variable.value)) && !value_is_global_const;

                AST_Implicit_LValue implicit_lval = { AST_Implicit_LValue_Kind::SLICE_ARRAY,
                                                      decl->variable.value,
                                                      .slice = {
                                                          .type = decl->variable.resolved_type,
                                                          .needs_local_array_alloc = needs_local_array_alloc,
                                                          .needs_global_array_alloc = value_is_global_const,
                                                      }
                                                    };

                if (scope->kind != Scope_Kind::GLOBAL) {
                    auto current_function = enclosing_function(scope);
                    dynamic_array_append(&current_function->function.implicit_lvalues, implicit_lval);
                } else {
                    auto flat_node = alloc<Flat_Root_Node>(ctx->resolver->node_allocator);
                    flat_node->root.kind = Flat_Node_Kind::IMPLICIT_LVALUE;
                    flat_node->root.implicit_lvalue = implicit_lval;
                    dynamic_array_insert(&ctx->resolver->nodes_to_emit_bytecode, flat_node);
                }
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
                    auto param_types = temp_array_create<Type *>(temp_allocator_allocator());

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
            if (decl->aggregate.resolved_type) {
                assert(decl->aggregate.resolved_type == get_string_type(ctx));
            } else {

                // auto temp_member_types = temp_array_create<Type *>(temp_allocator_allocator(), decl->aggregate.fields.count);
                Dynamic_Array<Type *> member_types;
                dynamic_array_create(&ctx->ast_allocator, &member_types, decl->aggregate.fields.count);

                for (s64 i = 0; i < decl->aggregate.fields.count; i++) {
                    auto field = decl->aggregate.fields[i];
                    assert(field->kind == AST_Declaration_Kind::FIELD);
                    assert(field->field.resolved_type);

                    dynamic_array_append(&member_types, field->field.resolved_type);
                }

                auto sym = scope_get_symbol(scope, decl->identifier.name);

                auto unfinished_type = sym->aggregate.unfinished_struct_type;
                if (unfinished_type) {
                    assert(unfinished_type->structure.name == decl->identifier.name);
                    decl->aggregate.resolved_type = finalize_struct_type(unfinished_type, member_types, &ctx->ast_allocator);
                } else {
                    decl->aggregate.resolved_type = get_struct_type(member_types, decl->identifier.name, &ctx->ast_allocator);
                }

            }

            auto sym = scope_get_symbol(scope, decl->identifier.name);
            assert(sym && sym->state == Symbol_State::RESOLVED);
            assert(sym->kind == Symbol_Kind::TYPE);
            sym->state = Symbol_State::TYPED;

            result = true;
            break;
        }

        case AST_Declaration_Kind::UNION: assert(false);

        case AST_Declaration_Kind::RUN_DIRECTIVE: {
            if (!run_directive_is_const(ctx, decl->directive)) { // This reports errors internally
                return false;
            }

            result = true;
            break;
        }

        case AST_Declaration_Kind::IMPORT_DIRECTIVE: {
            result = true;
            break;
        }
    }

    if (result) {
        decl->flags |= AST_DECL_FLAG_TYPED;
    }

    return result;
}

bool type_resolve_statement(Resolver *resolver, AST_Statement *stmt, Scope *scope)
{
    debug_assert(resolver && stmt && scope);

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
                fatal_resolve_error(resolver->ctx, lvalue_expr, "Left side of assignment must be an lvalue");
                result = false;
                break;
            }

            assert(lvalue_expr->resolved_type);
            assert(value_expr->resolved_type);

            if (value_expr->resolved_type != lvalue_expr->resolved_type) {
                if (valid_static_type_conversion(value_expr->resolved_type, lvalue_expr->resolved_type)) {
                    // ok
                } else {
                    resolve_error(resolver->ctx, value_expr, "Mismatching type in assignment");
                    resolve_error(resolver->ctx, lvalue_expr, "    Expected: %s", temp_type_string(lvalue_expr->resolved_type));
                    resolve_error(resolver->ctx, value_expr, "    Got: %s", temp_type_string(value_expr->resolved_type));
                    return false;
                }

                if (lvalue_expr->resolved_type->kind == Type_Kind::SLICE &&
                    value_expr->resolved_type->kind == Type_Kind::STATIC_ARRAY) {

                    bool value_is_global_const = false;

                    // Check if the value is pointing to a global (constant).
                    if (value_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                        auto ident_sym = scope_get_symbol(scope, value_expr->identifier.name);
                        auto ident_decl = ident_sym->decl;

                        if (ident_decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE) {
                            value_is_global_const = true;
                        }
                    }

                    bool needs_array_alloc = (value_expr->kind == AST_Expression_Kind::COMPOUND || EXPR_IS_CONST(value_expr)) && ! value_is_global_const;

                    auto current_function = enclosing_function(scope);
                    AST_Implicit_LValue implicit_lval = { AST_Implicit_LValue_Kind::SLICE_ARRAY,
                                                          value_expr,
                                                          .slice = { .type = lvalue_expr->resolved_type,
                                                                     .needs_local_array_alloc = needs_array_alloc,
                                                                     .needs_global_array_alloc = value_is_global_const,
                                                                   }
                                                        };

                    dynamic_array_append(&current_function->function.implicit_lvalues, implicit_lval);
                }
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

        case AST_Statement_Kind::WHILE: {

            auto cond = stmt->while_stmt.cond;

            if (cond->resolved_type->kind != Type_Kind::BOOLEAN) {
                fatal_resolve_error(resolver->ctx, cond, "Expected boolean type, got: %s", temp_type_string(cond->resolved_type).data);
                return false;
            }
            break;
        }

        case AST_Statement_Kind::FOR: {

            auto cond_expr = stmt->for_stmt.cond_expr;
            if (cond_expr->resolved_type->kind != Type_Kind::BOOLEAN) {
                fatal_resolve_error(resolver->ctx, cond_expr, "Expected boolean type in 'for' conditional, got: '%'", temp_type_string(cond_expr->resolved_type));
                return false;
            }
            break;
        }

        case AST_Statement_Kind::DEFER: {
            // Leaf
            break;
        }

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
                    AST_Expression *cast_expr = ast_cast_expr_new(resolver->ctx, value_expr->range, expected_type, value_expr);
                    bool cast_name_result = name_resolve_expr(resolver->ctx, cast_expr, scope);
                    assert(cast_name_result);
                    bool cast_type_result = type_resolve_expression(resolver, cast_expr, scope, nullptr);
                    assert(cast_type_result);
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

bool type_resolve_expression(Resolver *resolver, AST_Expression *expr, Scope *scope, Infer_Node *infer_type_from)
{
    debug_assert(resolver && expr);

    auto ctx = resolver->ctx;

    if (expr->resolved_type && !(expr->kind == AST_Expression_Kind::RUN_DIRECTIVE)) return true;

    debug_assert(scope);

    bool result = true;

    Type *inferred_type = nullptr;
    if (infer_type_from) {
        inferred_type = infer_type(ctx, infer_type_from, expr->range);
        if (!inferred_type) return false;
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL: {

                if (inferred_type) {

                    if (inferred_type->kind == Type_Kind::UNSIZED_INTEGER) {
                        inferred_type = &builtin_type_s64;
                    } if (inferred_type->kind == Type_Kind::FLOAT) {
                        // Ok
                    } else if (inferred_type->kind != Type_Kind::INTEGER) {
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
            auto string_type = get_string_type(ctx);
            assert(!inferred_type || inferred_type == string_type);
            expr->resolved_type = string_type;
            break;
        }

        case AST_Expression_Kind::CHAR_LITERAL: {
            assert(!inferred_type || inferred_type == &builtin_type_u8);
            expr->resolved_type = &builtin_type_u8;
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            assert(inferred_type);
            assert(inferred_type->kind == Type_Kind::POINTER);
            expr->resolved_type = inferred_type;
            break;
        }

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
            assert(EXPR_IS_TYPED(base_expr));

            bool via_pointer = false;
            auto aggregate_type = base_expr->resolved_type;
            if (aggregate_type->kind == Type_Kind::POINTER) {
                via_pointer = true;
                aggregate_type = aggregate_type->pointer.base;
            } else if (aggregate_type->kind == Type_Kind::SLICE) {
                aggregate_type = aggregate_type->slice.struct_type;
            } else if (aggregate_type->kind == Type_Kind::STATIC_ARRAY) {
                expr->resolved_type = &builtin_type_s64;
                break;
            }
            assert(aggregate_type->flags & TYPE_FLAG_AGGREGATE);

            auto type_sym = scope_get_symbol(scope, aggregate_type->structure.name);
            assert(type_sym);
            assert(type_sym->kind == Symbol_Kind::TYPE);

            assert(type_sym->aggregate.scope && type_sym->aggregate.scope->kind == Scope_Kind::AGGREGATE);
            auto sym = scope_get_symbol(type_sym->aggregate.scope, expr->member.member_name);
            assert(sym && sym->kind == Symbol_Kind::MEMBER);
            assert(sym->state == Symbol_State::TYPED);

            if (sym->flags & SYM_FLAG_BUILTIN) {
                assert(sym->builtin_type);
                expr->resolved_type = sym->builtin_type;
                expr->flags |= AST_EXPR_FLAG_LVALUE;
            } else {
                auto mem_decl = sym->decl;
                assert(mem_decl && mem_decl->kind == AST_Declaration_Kind::FIELD);
                assert(mem_decl->field.resolved_type);

                expr->resolved_type = mem_decl->field.resolved_type;

                if (via_pointer) {
                    if (EXPR_IS_LVALUE(base_expr)) {
                        expr->flags |= AST_EXPR_FLAG_LVALUE;
                    }
                } else {
                    assert(EXPR_IS_LVALUE(base_expr));
                    expr->flags |= AST_EXPR_FLAG_LVALUE;
                }
            }
            break;
        }

        case AST_Expression_Kind::INDEX: {
            auto base_expr = expr->index.base;
            assert(EXPR_IS_TYPED(base_expr));
            assert(EXPR_IS_LVALUE(base_expr));

            auto base_type = base_expr->resolved_type;

            bool base_is_array = base_expr->resolved_type->kind == Type_Kind::STATIC_ARRAY;
            bool base_is_slice = base_expr->resolved_type->kind == Type_Kind::SLICE;

            if (!base_is_array && !base_is_slice && base_expr->resolved_type != get_string_type(ctx)) {
                fatal_resolve_error(ctx, expr, "Base of index expression is not an array, slice or String");
                return false;
            }

            auto index_expr = expr->index.index;
            assert(EXPR_IS_TYPED(index_expr));
            assert(index_expr->resolved_type->kind == Type_Kind::INTEGER);

            if (base_is_array) {

                if (EXPR_IS_CONST(index_expr)) {
                    Integer_Value index_val = resolve_constant_integer_expr(index_expr);
                    assert(index_val.s64 >= 0 && index_val.s64 < base_type->static_array.count);
                }

                expr->resolved_type = base_type->static_array.element_type;

            } else if (base_is_slice) {

                expr->resolved_type = base_type->slice.element_type;

            } else {
                expr->resolved_type = &builtin_type_u8;
            }

            expr->flags |= AST_EXPR_FLAG_LVALUE;
            break;
        }

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
                        // ok
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

                if (param_type->kind == Type_Kind::SLICE && arg_type->kind == Type_Kind::STATIC_ARRAY) {

                    arg_expr->flags |= AST_EXPR_FLAG_SLICE_ARRAY;
                    auto current_fn = enclosing_function(scope);

                    bool value_is_global_const = false;

                    // Check if the value is pointing to a global (constant).
                    if (arg_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                        auto ident_sym = scope_get_symbol(scope, arg_expr->identifier.name);
                        auto ident_decl = ident_sym->decl;

                        if (ident_decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE) {
                            value_is_global_const = true;
                        }
                    }

                    bool needs_local_array_alloc = (arg_expr->kind == AST_Expression_Kind::COMPOUND || EXPR_IS_CONST(arg_expr)) && !value_is_global_const;

                    AST_Implicit_LValue implicit_lval = { AST_Implicit_LValue_Kind::SLICE_ARRAY,
                                                          arg_expr,
                                                          .slice = { .type = param_type,
                                                                     .needs_local_array_alloc = needs_local_array_alloc,
                                                                     .needs_global_array_alloc = value_is_global_const,
                                                                   }
                                                        };

                    dynamic_array_append(&current_fn->function.implicit_lvalues, implicit_lval);
                }
            }

            auto return_type = func_decl->function.type->function.return_type;
            assert(return_type);

            expr->resolved_type = return_type;
            break;
        }

        case AST_Expression_Kind::UNARY: {
            AST_Expression *operand = expr->unary.operand;
            assert(EXPR_IS_TYPED(operand));

            Type *base_type = operand->resolved_type;

            switch (expr->unary.op) {

                case AST_Unary_Operator::INVALID: assert(false); break;

                case AST_Unary_Operator::PLUS:
                case AST_Unary_Operator::MINUS: {
                    if (operand->resolved_type->kind != Type_Kind::INTEGER &&
                        operand->resolved_type->kind != Type_Kind::UNSIZED_INTEGER) {
                        fatal_resolve_error(ctx, operand, "Expected integer type after unary %c", expr->unary.op);
                        return false;
                    }

                    if (operand->resolved_type->kind == Type_Kind::INTEGER) {
                        if (expr->unary.op == AST_Unary_Operator::MINUS &&
                            !operand->resolved_type->integer.sign) {
                            fatal_resolve_error(ctx, operand, "Expected signed integer after unary '-'");
                            return false;
                        }
                    }

                    expr->resolved_type = operand->resolved_type;
                    break;
                }

                case AST_Unary_Operator::ADDRESS_OF: {

                    if (EXPR_IS_CONST(operand)) {
                        // The expression must have a declaration, probably add a function to test for this more thoroughly.
                        assert(operand->kind == AST_Expression_Kind::IDENTIFIER);

                        auto sym = scope_get_symbol(scope, operand->identifier);
                        assert(sym);
                        assert(sym->decl);
                        assert(sym->decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);

                        AST_Implicit_LValue implicit_lvalue = { AST_Implicit_LValue_Kind::CONST_LVALUE, operand, .decl = sym->decl };

                        if (scope->kind != Scope_Kind::FUNCTION_LOCAL && scope->kind != Scope_Kind::FUNCTION_PARAMETER) {
                            assert(scope->kind == Scope_Kind::GLOBAL);
                            auto flat_node = alloc<Flat_Root_Node>(resolver->node_allocator);
                            flat_node->root.kind = Flat_Node_Kind::IMPLICIT_LVALUE;
                            flat_node->root.implicit_lvalue = implicit_lvalue;
                            dynamic_array_insert(&ctx->resolver->nodes_to_emit_bytecode, flat_node);
                        } else {
                            auto current_function = enclosing_function(scope);
                            dynamic_array_append(&current_function->function.implicit_lvalues, implicit_lvalue);
                        }

                    } else if (!EXPR_IS_LVALUE(operand)) {

                        fatal_resolve_error(ctx, expr, "The operand to pointer-to ('*') is not an lvalue");
                        fatal_resolve_error(ctx, operand, "Operand: '%s'", ast_print_expression(operand, &ctx->error_allocator).data);
                        return false;
                    }

                    expr->resolved_type = get_pointer_type(base_type, &ctx->ast_allocator);
                    expr->flags |= AST_EXPR_FLAG_LVALUE;
                    break;
                }

                case AST_Unary_Operator::DEREF: {
                    auto operand = expr->unary.operand;
                    assert(operand->resolved_type->kind == Type_Kind::POINTER);

                    expr->resolved_type = operand->resolved_type->pointer.base;

                    if (EXPR_IS_LVALUE(operand)) {
                        expr->flags |= AST_EXPR_FLAG_LVALUE;
                    }
                    break;
                }
            }

            debug_assert(expr->resolved_type);

            break;
        }

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

        case AST_Expression_Kind::CAST: {
            assert(EXPR_IS_TYPED(expr->cast.value));

            Type *target_type = nullptr;
            if (expr->cast.type) {
                target_type = expr->cast.type;
            } else {
                assert(expr->cast.type_spec->resolved_type);
                target_type = expr->cast.type_spec->resolved_type;
            }
            assert(target_type);

            bool valid_conversion = false;
            if (target_type == expr->cast.value->resolved_type) {
                valid_conversion = true;
            } else {
                valid_conversion = valid_static_type_conversion(expr->cast.value->resolved_type, target_type);
            }
            assert(valid_conversion);

            expr->resolved_type = target_type;

            if (EXPR_IS_CONST(expr->cast.value)) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }
            break;
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            auto dir = expr->directive.directive;

            if (!EXPR_IS_TYPED(expr)) {

                if (!run_directive_is_const(ctx, expr->directive.directive)) { // This reports errors internally
                    return false;
                }

                Type *type = nullptr;

                switch (dir->run.kind) {

                    case AST_Run_Directive_Kind::INVALID: assert(false); break;
                    case AST_Run_Directive_Kind::EXPR: {
                        type = dir->run.expr->resolved_type;
                        if (type->kind == Type_Kind::UNSIZED_INTEGER) {
                            type = &builtin_type_s64;
                            dir->run.expr->resolved_type = &builtin_type_s64;
                        }
                        break;
                    }

                    case AST_Run_Directive_Kind::STMT: {
                        fatal_resolve_error(ctx, dir->run.stmt, "Expected expression after #run in assignment");
                        return false;
                    }
                }

                assert(type);

                auto node = alloc<Flat_Root_Node>(resolver->node_allocator);
                node->root.kind = Flat_Node_Kind::RUN;
                node->root.run.expr = expr;
                dynamic_array_append(&resolver->nodes_to_emit_bytecode, node);

                expr->resolved_type = type;
                expr->flags |= AST_EXPR_FLAG_TYPED;
                return false; // Always fail the first time around since we need to emit and execute the bytecode first

            } else {

                if (!expr->directive.generated_expression) {
                    resolve_error(ctx, expr, "Waiting for #run to be executed");
                    return false;
                }
            }
            break;
        }

        case AST_Expression_Kind::COMPOUND: {
            assert(inferred_type);

            auto compound_member_count = expr->compound.expressions.count;

            bool all_literal = true;
            bool all_const = true;

            if (inferred_type->flags & TYPE_FLAG_AGGREGATE) {
                auto aggregate_type = inferred_type;
                assert(aggregate_type->kind == Type_Kind::STRUCTURE);

                auto aggregate_member_count = aggregate_type->structure.member_types.count;

                if (aggregate_member_count != compound_member_count) {
                    fatal_resolve_error(ctx, expr, "Mismatching expression count in compound expression, expected %i, got %i",  aggregate_member_count, compound_member_count);
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

                    if (!(compound_member_expr->flags & AST_EXPR_FLAG_LITERAL)) {
                        all_literal = false;
                    }
                    if (!(compound_member_expr->flags & AST_EXPR_FLAG_CONST)) {
                        all_const = false;
                    }
                }

            } else if (inferred_type->kind == Type_Kind::STATIC_ARRAY) {
                auto array_type = inferred_type;
                auto array_member_count = array_type->static_array.count;

                if (array_member_count != compound_member_count) {
                    fatal_resolve_error(ctx, expr, "Mismatching expression count in compound expression, expected %i, got %i", array_member_count, compound_member_count);
                    return false;
                }

                auto array_element_type = array_type->static_array.element_type;

                for (s64 i = 0; i < array_member_count; i++) {
                    auto compound_member_expr = expr->compound.expressions[i];
                    Type *compound_member_type = compound_member_expr->resolved_type;

                    if (array_element_type != compound_member_type) {
                        fatal_resolve_error(ctx, compound_member_expr, "Mismatching type for compound member %i", i + 1);
                        fatal_resolve_error(ctx, compound_member_expr, "    Expected: %s", temp_type_string(array_element_type));
                        fatal_resolve_error(ctx, compound_member_expr, "    Got: %s", temp_type_string(compound_member_type));
                        return false;
                    }

                    if (!(compound_member_expr->flags & AST_EXPR_FLAG_LITERAL)) {
                        all_literal = false;
                    }
                    if (!(compound_member_expr->flags & AST_EXPR_FLAG_CONST)) {
                        all_const = false;
                    }
                }

            } else if (inferred_type->kind == Type_Kind::SLICE) {
                expr->flags |= AST_EXPR_FLAG_SLICE_ARRAY;
                auto slice_type = inferred_type;

                auto slice_element_type = slice_type->slice.element_type;

                for (s64 i = 0; i < compound_member_count; i++) {
                    auto compound_member_expr = expr->compound.expressions[i];
                    Type *compound_member_type = compound_member_expr->resolved_type;

                    if (slice_element_type != compound_member_type) {
                        fatal_resolve_error(ctx, compound_member_expr, "Mismatching type for compound member %i", i + 1);
                        fatal_resolve_error(ctx, compound_member_expr, "    Expected: %s", temp_type_string(slice_element_type));
                        fatal_resolve_error(ctx, compound_member_expr, "    Got: %s", temp_type_string(compound_member_type));
                        return false;
                    }

                    if (!(compound_member_expr->flags & AST_EXPR_FLAG_LITERAL)) {
                        all_literal = false;
                    }
                    if (!(compound_member_expr->flags & AST_EXPR_FLAG_CONST)) {
                        all_const = false;
                    }
                }

                inferred_type = get_static_array_type(inferred_type->slice.element_type, compound_member_count, &ctx->ast_allocator);
                expr->resolved_type = inferred_type;

            } else {
                assert_msg(false, "Invalid inferred type for compound expression");
            }

            expr->resolved_type = inferred_type;

            if (all_literal) {
                expr->flags |= AST_EXPR_FLAG_LITERAL;
            }
            if (all_const) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }
            break;
        }
    }

    assert(result);
    assert(expr->resolved_type);
    expr->flags |= AST_EXPR_FLAG_TYPED;

    return result;
}

bool type_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope, bool via_pointer)
{
    debug_assert(ctx && ts && scope);

    if (ts->resolved_type) return true;

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::TYPE: {
            assert(ts->resolved_type);
            return true;
            break;
        }

        case AST_Type_Spec_Kind::NAME: {
            auto sym = scope_get_symbol(scope, ts->identifier.name);

            if (sym->state != Symbol_State::TYPED) {

                if (via_pointer && sym->kind == Symbol_Kind::TYPE && sym->decl->kind == AST_Declaration_Kind::STRUCT) {

                    // Create the struct type so we can create a pointer to it...
                    // The members are filled in later.
                    Type *struct_type;
                    if (sym->aggregate.unfinished_struct_type) {
                        struct_type = sym->aggregate.unfinished_struct_type;
                    } else {
                        struct_type = alloc<Type>(&ctx->ast_allocator);
                        create_struct_type(struct_type, {}, sym->name);
                        struct_type->flags |= TYPE_FLAG_UNFINISHED_STRUCT_TYPE;
                        sym->aggregate.unfinished_struct_type = struct_type;
                    }

                    ts->resolved_type = struct_type;
                    return true;

                } else {
                    resolve_error(ctx, sym->decl, "Waiting for symbol to be typed");
                    return false;
                }
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

        case AST_Type_Spec_Kind::POINTER: {
            assert(ts->pointer_base->resolved_type);

            auto pointer_type = get_pointer_type(ts->pointer_base->resolved_type, &ctx->ast_allocator);
            ts->resolved_type = pointer_type;
            return true;
        }

        case AST_Type_Spec_Kind::STATIC_ARRAY: {
            auto length_expr = ts->static_array.length_expr;
            assert(EXPR_IS_TYPED(length_expr));
            assert(EXPR_IS_CONST(length_expr));
            assert(length_expr->resolved_type->kind == Type_Kind::INTEGER);
            assert(length_expr->resolved_type == &builtin_type_s64); // This might not be the case when using a identifier (pointing to constant)

            Integer_Value length_val = resolve_constant_integer_expr(length_expr);
            assert(length_val.s64 >= 1);

            auto elem_ts = ts->static_array.element_ts;
            assert(elem_ts->resolved_type);
            auto elem_type = elem_ts->resolved_type;

            ts->resolved_type = get_static_array_type(elem_type, length_val.s64, &ctx->ast_allocator);
            return true;
        }

        case AST_Type_Spec_Kind::SLICE: {
            auto elem_ts = ts->slice.element_ts;
            assert(elem_ts->resolved_type);
            auto elem_type = elem_ts->resolved_type;

            ts->resolved_type = get_slice_type(ctx,elem_type, &ctx->ast_allocator);
            return true;
        }
    }

    assert(false);
    return false;
}

bool run_directive_is_const(Zodiac_Context *ctx, AST_Directive *dir) {

    debug_assert(ctx && dir);
    assert(dir->kind == AST_Directive_Kind::RUN);

    if (dir->run.expr) {
        return run_directive_expr_is_const(ctx, dir->run.expr);

    } else {
        assert(dir->run.stmt);
        return run_directive_stmt_is_const(ctx, dir->run.stmt);
    }

    assert(false);
    return false;
}

bool run_directive_expr_is_const(Zodiac_Context *ctx, AST_Expression *expr)
{
    debug_assert(ctx && expr);

    if (expr->kind == AST_Expression_Kind::CALL) {

        for (s64 i = 0; i < expr->call.args.count; i++) {
            if (!EXPR_IS_CONST(expr->call.args[i])) {
                fatal_resolve_error(ctx, expr->call.args[i], "Run directive expression must be constant");
                return false;
            }
        }

    } else if (!EXPR_IS_CONST(expr)) {
        fatal_resolve_error(ctx, expr, "Run directive expression must be constant");
        return false;
    }

    return true;
}

bool run_directive_stmt_is_const(Zodiac_Context *ctx, AST_Statement *stmt)
{
    debug_assert(ctx && stmt);

    switch (stmt->kind) {

        default: {
            fatal_resolve_error(ctx, stmt, "Only print and call statements are allowed in run blocks");
            return false;
        }

        case AST_Statement_Kind::PRINT: {
            for (s64 i = 0; i < stmt->print_expr.expressions.count; i++) {
                auto expr = stmt->print_expr.expressions[i];
                if (!EXPR_IS_CONST(expr)) {
                    fatal_resolve_error(ctx, expr, "Arguments to print in #run must be constant");
                    return false;
                }
            }
            return true;
        }

        case AST_Statement_Kind::BLOCK: {
            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                auto mem_stmt = stmt->block.statements[i];
                if (!run_directive_stmt_is_const(ctx, mem_stmt)) {
                    return false;
                }
            }
            return true;
        }

        case AST_Statement_Kind::CALL: {
            return run_directive_expr_is_const(ctx, stmt->call.call);
        }
    }

    assert(false);
    return false;
}


}
